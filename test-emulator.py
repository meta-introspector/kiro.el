#!/usr/bin/env python3
"""Test FRACTRAN emulator in browser with Selenium."""

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import time
import os

def test_emulator():
    # Get absolute path to HTML file
    html_path = os.path.abspath("fractran-emulator.html")
    url = f"file://{html_path}"
    
    print(f"🧪 Testing FRACTRAN emulator: {url}")
    
    # Setup Chrome driver
    options = webdriver.ChromeOptions()
    options.add_argument('--headless')
    options.add_argument('--no-sandbox')
    options.add_argument('--disable-dev-shm-usage')
    
    driver = webdriver.Chrome(options=options)
    
    try:
        # Load page
        driver.get(url)
        print("✓ Page loaded")
        
        # Wait for auto-run to complete
        time.sleep(1)
        
        # Check initial state (+ 1 2)
        cid = driver.find_element(By.ID, "cid").text
        shard = driver.find_element(By.ID, "shard").text
        harmonic = driver.find_element(By.ID, "harmonic").text
        
        print(f"✓ Initial run: CID={cid}, Shard={shard}, Harmonic={harmonic}")
        
        # Test example buttons
        examples = ['add', 'multiply', 'factorial']
        for ex in examples:
            driver.find_element(By.XPATH, f"//button[contains(text(), '{ex.title()}')]").click()
            time.sleep(0.5)
            
            # Run FRACTRAN
            driver.find_element(By.XPATH, "//button[contains(text(), 'Run FRACTRAN')]").click()
            time.sleep(0.5)
            
            # Check output updated
            trace = driver.find_element(By.ID, "trace-output").text
            assert "Step" in trace, f"No trace for {ex}"
            print(f"✓ Example '{ex}' works")
        
        # Test custom input
        input_box = driver.find_element(By.ID, "elisp-input")
        input_box.clear()
        input_box.send_keys("(+ 13 57)")
        
        driver.find_element(By.XPATH, "//button[contains(text(), 'Run FRACTRAN')]").click()
        time.sleep(0.5)
        
        # Verify Monster classification
        shard_val = int(driver.find_element(By.ID, "shard").text)
        assert 0 <= shard_val < 71, f"Invalid shard: {shard_val}"
        
        sephirah = driver.find_element(By.ID, "sephirah").text
        assert sephirah in ['Kether', 'Chokmah', 'Binah', 'Chesed', 'Geburah', 
                            'Tiphareth', 'Netzach', 'Hod', 'Yesod', 'Malkuth']
        
        print(f"✓ Custom input (+ 13 57): Shard={shard_val}, Sephirah={sephirah}")
        
        # Check all panels exist
        panels = driver.find_elements(By.CLASS_NAME, "panel")
        assert len(panels) == 4, f"Expected 4 panels, got {len(panels)}"
        print(f"✓ All 4 panels present")
        
        print("\n✅ All tests passed!")
        return True
        
    except Exception as e:
        print(f"\n❌ Test failed: {e}")
        # Take screenshot on failure
        driver.save_screenshot("test-failure.png")
        print("Screenshot saved: test-failure.png")
        return False
        
    finally:
        driver.quit()

if __name__ == "__main__":
    success = test_emulator()
    exit(0 if success else 1)
