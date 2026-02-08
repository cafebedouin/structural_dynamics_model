% ============================================================================
% CONSTRAINT STORY: trade_secret_law
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Uniform Trade Secrets Act (UTSA) / Defend Trade Secrets Act (DTSA)
% ============================================================================

:- module(constraint_trade_secret_law, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: trade_secret_law
 * human_readable: Trade Secret Law (Information Ownership)
 * domain: legal/economic/technological
 * temporal_scope: Civilizational (Permanent feature of commerce)
 * spatial_scope: Global
 * 
 * SUMMARY:
 * Trade Secret Law protects confidential business information (formulae, 
 * practices, designs, instruments, or patterns) that provides an enterprise 
 * a competitive advantage. It exists as long as the information remains 
 * secret and the owner takes reasonable steps to protect it.
 * 
 * KEY AGENTS:
 * - The Accused Whistleblower (Individual Powerless): Faces litigation for alleged misappropriation.
 * - The Courts (Institutional): The final arbiters of what constitutes "misappropriation."
 * - The Information Owner (Individual Powerful): The firm that invested in the secret.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(trade_secret_law, 0, 10).
narrative_ontology:constraint_claim(trade_secret_law, tangled_rope).

% Base extractiveness: 0.2.
% It protects the firm's investment without extracting the employee's 
% general right to work or earn a living.
domain_priors:base_extractiveness(trade_secret_law, 0.2).

% Suppression: 0.3.
% The rules are generally transparent, though the specific "secrets" 
% are (by definition) suppressed/hidden.
domain_priors:suppression_score(trade_secret_law, 0.3).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(trade_secret_law, extractiveness, 0.2).
narrative_ontology:constraint_metric(trade_secret_law, suppression_requirement, 0.3).

% Enforcement: Emerges naturally as a market norm and legal baseline.
domain_priors:emerges_naturally(trade_secret_law).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(trade_secret_law, innovative_enterprises).
constraint_victim(trade_secret_law, industrial_spies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ACCUSED WHISTLEBLOWER - Snare
   --------------------------------------------------------------------------
   WHO: powerless (Faces litigation for alleged misappropriation)
   WHEN: immediate (The moment of legal action)
   WHERE: trapped (By high legal costs and ambiguity of "secret" definition)
   
   WHY THIS CLASSIFICATION:
   If a company uses "Trade Secret" claims to suppress a whistleblower 
   or block a departing employee's move through litigation, the law 
   becomes a 'Snare'. The high cost of legal defense and the ambiguity 
   of what constitutes a "secret" can strangle the individual's 
   career and mobility, even if their actions were ethically motivated.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    trade_secret_law,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE COURTS - Rope
   --------------------------------------------------------------------------
   WHO: institutional (The final arbiters of what constitutes "misappropriation")
   WHEN: historical (Evolving jurisprudence in intellectual property)
   WHERE: arbitrage (Balances competing interests of innovation, competition, and mobility)
   
   WHY THIS CLASSIFICATION:
   For the Courts, trade secret law is a 'Rope'—a tool for balancing innovation,
   competition, and individual mobility. It provides a framework for adjudicating
   disputes, promoting fair business practices, and fostering a climate of trust
   and innovation within the economy.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    trade_secret_law,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE INFORMATION OWNER - Mountain
   --------------------------------------------------------------------------
   WHO: powerful (The firm that invested in the secret)
   WHEN: historical (Securing competitive advantage over decades)
   WHERE: arbitrage (Uses legal protection to maintain market position)
   
   WHY THIS CLASSIFICATION:
   For an information owner, Trade Secret Law is a 'Mountain'—a permanent feature 
   of the intellectual property landscape. They build their company's "moat" based
   on these protections, viewing it as an immutable law of the marketplace that
   allows them to secure venture capital by proving their value is protected.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    trade_secret_law,
    mountain,
    context(
        agent_power(powerful),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(trade_secret_law_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(trade_secret_law, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trade_secret_law, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(trade_secret_law, Type3, context(agent_power(powerful), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(trade_secret_law_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini Pro (Revised)
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'The Courts' as the institutional agent.
 *    For them, trade secret law is a 'Rope' for balancing competing interests.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Accused Whistleblower (Snare): Trapped by legal costs and ambiguity.
 *    - The Courts (Rope): A tool for balancing competing interests.
 *    - Information Owner (Mountain): Immutable legal protection for IP.
 * 
 * 3. CORE INSIGHT: Trade secret law is a 'Tangled Rope'. It's a 'Rope' for
 *    innovative firms, protecting their intellectual assets and fostering
 *    economic growth. However, it can become a 'Snare' for individuals
 *    (like whistleblowers or departing employees) when used aggressively,
 *    highlighting the delicate balance between corporate protection and
 *    individual rights.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the potential for trade secret law to function as a de-facto non-compete.
 */

omega_variable(
    inevitable_disclosure_drift,
    "Will courts increasingly use the 'Inevitable Disclosure Doctrine' to treat Trade Secret Law as a de-facto Non-Compete agreement, effectively turning this 'Rope' into a 'Snare' for high-skill labor mobility?",
    resolution_mechanism("Monitoring case law trends in states with and without strong 'Inevitable Disclosure' doctrines (e.g., New York vs. California); legislative efforts to clarify or limit the doctrine's scope."),
    impact("If Yes: The 'Mountain' of legal precedent turns into a 'Snare' for all high-skill labor. If No: It remains a balanced 'Tangled Rope'."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Non-Compete Agreements
 *    Viability: Explicit contractual agreements restricting an employee's ability to work for competitors.
 *    Suppression: Increasingly suppressed by courts and legislatures (e.g., California) due to concerns about worker mobility and economic stagnation.
 *
 * CONCLUSION:
 * Trade secret law aims to be a 'Rope' that balances protection for firms with
 * mobility for employees. However, the potential for its aggressive application
 * (e.g., via "Inevitable Disclosure") risks transforming it into a 'Snare',
 * effectively replacing suppressed non-compete agreements with a more subtle form
 * of labor market restriction.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/trade_secret_law].
 * 2. Multi-perspective: ?- multi_index_report(trade_secret_law).
 * 3. Run tests: ?- run_tests(trade_secret_law_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    domain_priors:theater_ratio/2.

% --- Theater ratio (missing from base properties) ---
% Mixed coordination/extraction — theater masks extraction component
domain_priors:theater_ratio(trade_secret_law, 0.3).
narrative_ontology:constraint_metric(trade_secret_law, theater_ratio, 0.3).

% --- Analytical perspective classification (missing) ---
% chi = 0.2 * 1.15 (analytical) * 1.2 (global) = 0.276
% Classification: scaffold
constraint_indexing:constraint_classification(trade_secret_law, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Enforcement flag (required for tangled_rope gate) ---
% Tangled rope requires: constraint_beneficiary + constraint_victim + requires_active_enforcement
domain_priors:requires_active_enforcement(trade_secret_law).
