#!/bin/bash

# Run tests with coverage
busted -c

# Generate coverage report
luacov

# Display report summary
echo "Coverage report generated at luacov.report.out"
