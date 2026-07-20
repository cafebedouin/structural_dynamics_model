% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__orthodox_price_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__orthodox_price_stability, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ecb_mandate_article_127__orthodox_price_stability
 *   human_readable: ECB Article 127 Orthodox Price Stability Mandate
 *   domain: monetary_policy/constitutional_law/eu_governance
 *
 * SUMMARY:
 *   This constraint instantiates the orthodox_price_stability reading of the
 *   ecb_mandate_article_127 kernel (Article 127(1) TFEU). The kernel text
 *   states that price stability is the primary objective of the ECB, 'without
 *   prejudice to the objectives of the Union'. This reading interprets
 *   'without prejudice' as purely declaratory, rendering secondary objectives
 *   legally and operationally subordinate. Sibling
 *   readingsâexpansive_secondary_objectives and
 *   climate_incorporationâtreat the same text as permitting discretionary
 *   balancing or requiring climate-risk integration. The structural delta of
 *   this reading is a narrow beneficiary set (net creditor states and
 *   euro-area savers), externalized climate risks, and high institutional
 *   suppression of mandate expansion through treaty interpretation, Governing
 *   Council voting dynamics, and legal challenges.
 *
 * KEY AGENTS:
 *   - ECB Governing Council: agenda setter (institutional/identity_locked)âenforces the orthodox reading as constitutive of central-banking identity
 *   - Net creditor member states: primary beneficiary (powerful/constrained)âpreserve real value of claims and block fiscal transfer risk
 *   - Peripheral sovereign borrowers: primary target (powerful/constrained)âbear asymmetric adjustment and high real debt burdens
 *   - Indebted households: secondary target (powerless/trapped)âcarry mortgage and consumer debt under restrictive nominal conditions
 *   - Green policy coalition: excluded voice (organized/constrained)âstructurally absent from operational decisions
 *   - European Court of Justice: observer (institutional/analytical)âadjudicates mandate boundaries, generally upholding the orthodox fence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, 0.66).
domain_priors:suppression_score(ecb_mandate_article_127__orthodox_price_stability, 0.82).
domain_priors:theater_ratio(ecb_mandate_article_127__orthodox_price_stability, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, extractiveness, 0.66).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__orthodox_price_stability, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__orthodox_price_stability, "ECB Article 127 Orthodox Price Stability Mandate").
narrative_ontology:topic_domain(ecb_mandate_article_127__orthodox_price_stability, "monetary_policy/constitutional_law/eu_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__orthodox_price_stability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__orthodox_price_stability, '4d4a39d5-f649-4080-a074-3c3608ebdd55').
narrative_ontology:cs_kernel_codification('4d4a39d5-f649-4080-a074-3c3608ebdd55', fixed_text).
narrative_ontology:cs_authority_grounding('4d4a39d5-f649-4080-a074-3c3608ebdd55', lineage).
narrative_ontology:cs_interpretation_layer_present('4d4a39d5-f649-4080-a074-3c3608ebdd55').
narrative_ontology:cs_reading_relation('4d4a39d5-f649-4080-a074-3c3608ebdd55', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_reading_relation('4d4a39d5-f649-4080-a074-3c3608ebdd55', ecb_mandate_article_127__climate_incorporation, coexists_with).
narrative_ontology:cs_axiom('4d4a39d5-f649-4080-a074-3c3608ebdd55', foundational, price_stability_exclusive_mandate).
narrative_ontology:cs_axiom_status(price_stability_exclusive_mandate, holdable).
narrative_ontology:cs_axiom_grounding('4d4a39d5-f649-4080-a074-3c3608ebdd55', price_stability_exclusive_mandate, conventional).
narrative_ontology:cs_axiom('4d4a39d5-f649-4080-a074-3c3608ebdd55', foundational, secondary_objectives_non_operational).
narrative_ontology:cs_axiom_status(secondary_objectives_non_operational, holdable).
narrative_ontology:cs_axiom_grounding('4d4a39d5-f649-4080-a074-3c3608ebdd55', secondary_objectives_non_operational, conventional).
narrative_ontology:cs_reference_frame('4d4a39d5-f649-4080-a074-3c3608ebdd55', classical_price_stability_framework).
narrative_ontology:cs_drift_state('4d4a39d5-f649-4080-a074-3c3608ebdd55', post_climate_debate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4d4a39d5-f649-4080-a074-3c3608ebdd55', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, net_creditor_states).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, euro_area_savers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, peripheral_sovereign_borrowers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, indebted_households).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, climate_exposed_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers euro-area monetary policy under a treaty mandate interpreted as requiring exclusive focus on the 2% inflation target. Sets interest rates, asset purchase eligibility, and collateral frameworks, actively subordinating employment, growth, and climate objectives. Members are drawn from a hawkish epistemic community for whom price stability is constitutive of central-banking identity; reinterpretation of the mandate is treated as institutional betrayal.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council, agenda_setter,
    institutional, generational, identity_locked, continental).

% Member states with large net foreign asset positions within the euro area. Price stability and low inflation preserve the real value of their claims and minimize fiscal transfer risk to debtor states. They exercise agenda influence through ECB nomination processes, informal political pressure, and litigation supporting a narrow mandate.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, net_creditor_states, beneficiary,
    powerful, generational, constrained, continental).

% Households and institutional asset holders with nominal claims denominated in euro. Low and stable inflation protects the purchasing power of savings deposits and fixed-income portfolios. Portfolio diversification offers limited exit, but relocation or currency hedging is costly for retail agents.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, euro_area_savers, beneficiary,
    moderate, biographical, mobile, continental).

% High-debt euro-area member states reliant on market financing. The exclusive inflation target and associated restrictive macroeconomic bias raise real interest rates and debt-servicing costs, compressing fiscal space for public investment and automatic stabilizers. Euro exit remains catastrophic, leaving internal adjustment as the only viable path.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, peripheral_sovereign_borrowers, payer,
    powerful, generational, constrained, national).

% Households in high-debt regions carrying variable-rate or refinanced mortgages. Low-inflation environments with elevated real rates increase debt burdens and depress labor-market recovery. They cannot exit the monetary union and lack organized political leverage to alter the ECB's mandate.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, indebted_households, payer,
    powerless, biographical, trapped, national).

% Member states and regions whose productive capacity and fiscal balances are most exposed to climate transition costs. The exclusion of climate risk from monetary policy collateral frameworks and asset purchases externalizes transition financing onto their national budgets and private investment gaps.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, climate_exposed_economies, payer,
    moderate, generational, constrained, continental).

% Environmental ministries, MEPs, and civil society coalitions advocating integration of climate risk into ECB operations. They are structurally excluded from Governing Council deliberations; their proposals are treated as ultra vires under the orthodox reading and relegated to non-binding political dialogue.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, green_policy_coalition, excluded,
    organized, biographical, constrained, continental).

% The EU's highest court adjudicates challenges to ECB action. In key cases it has upheld the orthodox boundary of mandate interpretation, treating price stability as the legally cognizable primary objective and leaving secondary objectives as programmatic background without operational justiciability.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, european_court_of_justice, observer,
    institutional, civilizational, analytical, continental).

narrative_ontology:fixing_cost_class(ecb_mandate_article_127__orthodox_price_stability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates inflation expectations and macroeconomic policy across a heterogeneous monetary union, providing a nominal anchor that prevents fragmentation of the euro area into divergent national monetary policies and expectation regimes.
% TRANSFER_FUNCTION: Moves the burden of macroeconomic adjustment from creditor to debtor regions by subordinating full-employment and growth objectives to price stability; externalizes climate transition costs by excluding environmental criteria from monetary operations and collateral frameworks.
% ABSENT_VOICES: Debtor-country finance ministers, environmental ministers, and progressive MEPs are present in EU discourse but structurally excluded from ECB Governing Council decision-making; their absence means demand-side and climate perspectives enter only as litigation or political pressure, never as operational inputs.
% DISAPPEARANCE_RATIONALE: If the exclusive 2% target and its subordination clause vanished, ECB policy would likely adopt a dual or triple mandate; sovereign spreads would reprice as markets anticipated sustained lower real rates, fiscal space in peripheral states would expand, and green asset purchases would enter the operational toolkitâthe macroeconomic and political economy of the euro area would reorganize.
% FOUNDING_PROBLEM: A monetary union without a credible nominal anchor risks inflation bias, free-rider fiscal policies, and divergent national monetary stances that threaten the union's survival.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and monetary-union scholars attest the nominal-anchor problem was genuine in the 1990s. However, contemporary post-Keynesian and Keynesian economists outside the creditor-state bloc argue the problem has reversed into a deflationary-bias and asymmetric-shock trap; no independent corroboration from outside the benefiting parties supports the claim that the founding problem remains live in its original form.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__orthodox_price_stability, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__orthodox_price_stability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__orthodox_price_stability, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ecb_mandate_article_127__orthodox_price_stability, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__orthodox_price_stability, 0.66, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.66 over the interval because the low-inflation environment after 2008 increasingly transferred adjustment costs to debtor regions while excluding climate and employment objectives. Suppression rises from 0.40 to 0.82 as the hawkish coalition built legal and institutional firewalls against mandate expansion (Weiss/Gauweiler litigation, OMT conditionality, anti-climate-policy rhetoric). Theater rises from 0.20 to 0.45 because the 'independent technocrat' framing increasingly performs legitimacy work for a distributional choice that favors creditors. Accessibility collapse is moderate (0.60): alternatives such as dual mandates or green QE are visible in public discourse but structurally marginalized. Resistance is moderate (0.55): debtor states and climate coalitions push back, but are contained by treaty interpretation and creditor-state political economy.
 *
 * PERSPECTIVAL GAP:
 *   From the Frankfurt/ECB seat, the constraint is legitimate treaty fidelity and a necessary nominal anchor for monetary union. From Rome, Athens, or the climate-coalition seat, the same structure operates as a creditor-biased extraction mechanism that suppresses democratically legitimate objectives. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The ECB Governing Council sits near the beneficiary end structurally (it administers the constraint and its identity is fused with it), but because it does not personally capture the gains, it is best understood as an identity-locked agenda setter. Net creditor states and savers are clear beneficiaries (low d, subsidized by the constraint). Peripheral sovereign borrowers and indebted households are clear targets (high d, amplified extraction). Climate-exposed economies are targets via exclusion. The green policy coalition is excluded rather than coordinatedâtheir absence is a structural feature of the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâa credible nominal anchor for monetary unionâwas genuine in the 1990s. However, after decades of lowflation, asymmetric demand shortfalls, and climate transition needs, the arrangement persists beyond its original problem domain. The R5 genealogy flags this as contested: the hawkish coalition claims the problem is eternally live, while critics argue the constraint has become a zombie serving creditor interests. The temporal measurements show rising extraction and theater, consistent with a tangled rope sliding toward snare-like behavior as its coordination function atrophies relative to its distributional function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_secondary_objectives_ambiguity,
    'Does Article 127(1) TFEU (''without prejudice to the objectives of the Union'') logically permit operational weight for secondary objectives, or does the orthodox reading of exclusive focus represent the only legally coherent interpretation?',
    'ECJ preliminary ruling or treaty revision clarifying the hierarchical relationship between ''primary objective'' and ''without prejudice'' clauses.',
    'If the text permits operational secondary objectives, the orthodox reading is an interpretive choice rather than legal necessity, supporting reclassification toward extraction-dominant types; if exclusive focus is the only coherent reading, the constraint''s coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_secondary_objectives_ambiguity, conceptual, 'Ambiguity in treaty text regarding subordination of secondary objectives').

omega_variable(
    founding_problem_obsolescence,
    'Has the original founding problemâinflation bias in a monetary unionâbeen superseded by a deflationary bias and asymmetric demand shortfalls, rendering the exclusive price-stability constraint obsolete or even harmful?',
    'Cross-country panel analysis of Phillips curve stability, inflation expectations anchoring, and debt-deflation risk in the euro area since 2008.',
    'If the founding problem is dead, the constraint persists as a piton or snare serving creditor interests; if still live, the tangled-rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the original nominal-anchor problem has reversed into deflationary bias').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__orthodox_price_stability, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ecb__tr_t5, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 5, 0.22).
narrative_ontology:measurement(ecb__tr_t10, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 10, 0.28).
narrative_ontology:measurement(ecb__tr_t15, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 15, 0.35).
narrative_ontology:measurement(ecb__tr_t20, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 20, 0.38).
narrative_ontology:measurement(ecb__tr_t25, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 25, 0.42).
narrative_ontology:measurement(ecb__tr_t30, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ecb__be_t5, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(ecb__be_t10, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(ecb__be_t15, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(ecb__be_t20, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(ecb__be_t25, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(ecb__be_t30, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 30, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ecb__su_t5, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(ecb__su_t10, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(ecb__su_t15, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(ecb__su_t20, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(ecb__su_t25, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(ecb__su_t30, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 30, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__orthodox_price_stability, resource_allocation).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, climate_incorporation).

% DUAL FORMULATION NOTE:
% The ecb_mandate_article_127 kernel (Article 127(1) TFEU) decomposes into three structurally distinct constraints under the epsilon-invariance principle. The orthodox_price_stability reading treats the kernel as requiring exclusive focus on inflation; expansive_secondary_objectives treats the same text as permitting operational balancing; climate_incorporation treats it as requiring climate integration. They are not the same constraint viewed from different anglesâtheir epsilon values, beneficiary structures, and victim sets differ widely.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
