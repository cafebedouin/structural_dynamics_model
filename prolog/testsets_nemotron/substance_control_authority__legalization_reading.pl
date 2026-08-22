% ============================================================================
% CONSTRAINT STORY: substance_control_authority__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__legalization_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: substance_control_authority__legalization_reading
 *   human_readable: State Authority to Regulate Drug Markets as Legal Commerce
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the legalization reading of the
 *   substance_control_authority kernel: the state claims authority to
 *   regulate drug markets as legal commerce with quality and access controls.
 *   It displaces criminal prohibition with a licensed market structure. The
 *   reading's structural delta: users exit criminal victim sets and
 *   unregulated-supply victim sets; third parties are protected via market
 *   regulation; illicit markets are eliminated through competitive
 *   displacement; regulatory capacity is the primary mechanism; potential
 *   increase in use volume is accepted as a tradeoff. This is one of three
 *   sibling readings (prohibition_reading, harm_reduction_reading,
 *   legalization_reading) contesting the same kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__legalization_reading, 0.32).
domain_priors:suppression_score(substance_control_authority__legalization_reading, 0.41).
domain_priors:theater_ratio(substance_control_authority__legalization_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__legalization_reading, "State Authority to Regulate Drug Markets as Legal Commerce").
narrative_ontology:topic_domain(substance_control_authority__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__legalization_reading, 'f658c5d8-abbe-4cc2-b30d-7922d548c881').
narrative_ontology:cs_kernel_codification('f658c5d8-abbe-4cc2-b30d-7922d548c881', formalized).
narrative_ontology:cs_authority_grounding('f658c5d8-abbe-4cc2-b30d-7922d548c881', extraction).
narrative_ontology:cs_interpretation_layer_present('f658c5d8-abbe-4cc2-b30d-7922d548c881').
narrative_ontology:cs_reading_relation('f658c5d8-abbe-4cc2-b30d-7922d548c881', substance_control_authority__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('f658c5d8-abbe-4cc2-b30d-7922d548c881', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('f658c5d8-abbe-4cc2-b30d-7922d548c881', foundational, commercial_regulation_displaces_illicit_markets).
narrative_ontology:cs_axiom_status(commercial_regulation_displaces_illicit_markets, holdable).
narrative_ontology:cs_axiom_grounding('f658c5d8-abbe-4cc2-b30d-7922d548c881', commercial_regulation_displaces_illicit_markets, empirically_contingent).
narrative_ontology:cs_axiom('f658c5d8-abbe-4cc2-b30d-7922d548c881', foundational, state_revenue_justifies_regulatory_authority).
narrative_ontology:cs_axiom_status(state_revenue_justifies_regulatory_authority, holdable).
narrative_ontology:cs_axiom_grounding('f658c5d8-abbe-4cc2-b30d-7922d548c881', state_revenue_justifies_regulatory_authority, conventional).
narrative_ontology:cs_axiom('f658c5d8-abbe-4cc2-b30d-7922d548c881', secondary, quality_control_requires_licensed_commercial_supply).
narrative_ontology:cs_axiom_status(quality_control_requires_licensed_commercial_supply, holdable).
narrative_ontology:cs_axiom_grounding('f658c5d8-abbe-4cc2-b30d-7922d548c881', quality_control_requires_licensed_commercial_supply, instrumental).
narrative_ontology:cs_reference_frame('f658c5d8-abbe-4cc2-b30d-7922d548c881', prohibition_failure_recognized).
narrative_ontology:cs_drift_state('f658c5d8-abbe-4cc2-b30d-7922d548c881', post_legalization_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f658c5d8-abbe-4cc2-b30d-7922d548c881', '').
narrative_ontology:cs_kernel_id(substance_control_authority__legalization_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, legal_market_operators).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, consumer_users).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, tax_authorities).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, public_health_agencies).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, illicit_market_participants).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, high_risk_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, third_party_public).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, consumer_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces the licensing, quality testing, potency labeling, and access control framework for legal drug markets. Collects license fees and tax revenue. Bears the administrative burden of building and maintaining regulatory capacity. Can modify rules through rulemaking but faces political and institutional inertia.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, state_regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Licensed cultivators, processors, and retailers who gain legal market access and brand protection. Pay licensing fees, compliance costs, and excise taxes. Benefit from barriers to entry that limit competition. Exit requires abandoning sunk capital in facilities and brand; constrained by capital lock-in and regulatory capture incentives.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, legal_market_operators, beneficiary,
    organized, biographical, constrained, national).

% Adults who purchase regulated products. Gain known potency, purity, labeling, and legal possession. Pay retail prices including excise taxes and regulatory cost pass-through. Can exit to illicit market (price arbitrage) or cease use; mobile exit options but with quality/legal risk tradeoffs.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, consumer_users, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__legalization_reading, consumer_users, payer).

% Collect excise taxes, sales taxes, and license fees from the legal market. Revenue funds regulatory operations and general treasury. No direct operational role in market regulation. Exit is not applicable — the state's fiscal interest persists.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, tax_authorities, beneficiary,
    institutional, generational, analytical, national).

% Receive data from legal market (sales volumes, potency trends, adverse event reports) to inform prevention and treatment. Gain regulatory leverage (product standards, warning labels, purchase limits). Bear responsibility for health outcomes. Institutional exit not applicable; mandate persists.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, public_health_agencies, beneficiary,
    institutional, generational, analytical, national).

% Unlicensed cultivators, traffickers, and retailers displaced by legal market. Lose market share to regulated competitors who can operate openly. Face continued criminal enforcement. Constrained exit: can attempt to transition to legal market (capital, compliance barriers) or persist illicitly (enforcement risk). Not beneficiaries of the regulatory framework.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, illicit_market_participants, payer,
    organized, biographical, constrained, national).

% Users with severe substance use disorders who face access barriers in legal market (purchase limits, ID requirements, price floors, stigma). May be priced out or excluded by design. Trapped: cannot easily reduce consumption due to dependence; illicit market may be only accessible supply. Bear concentrated harm from both regulated market exclusions and residual illicit market dangers.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, high_risk_users, payer,
    powerless, immediate, trapped, national).

% Non-using public protected by regulations on advertising, public consumption, impaired driving, and youth access. Gains reduced visible drug-related disorder and crime. No direct cost; benefits from regulatory externalities. Analytical exit: the protection is a public good they cannot individually opt out of.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, third_party_public, beneficiary,
    moderate, generational, analytical, national).

% Monitor whether legalization delivers promised health benefits or creates new harms (commercialization, increased use). Advocate for lower barriers, safe supply, decriminalization of possession. Analytical seat: they influence policy but do not administer markets or bear direct costs.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, harm_reduction_advocates, observer,
    organized, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces criminal prohibition with a state-administered market that solves: product safety (unknown potency/adulterants), consumer information (labeling), revenue capture (taxation), and displacement of violent illicit markets — all through a single regulatory framework.
% TRANSFER_FUNCTION: Moves: (1) consumer spending from illicit to legal market operators (retail markup + tax), (2) regulatory compliance costs from operators to state agencies (license fees), (3) tax revenue from market to treasury, (4) health data from market to public health agencies. Illicit market participants lose revenue; high-risk users may face higher effective prices or access barriers.
% ABSENT_VOICES: Incarcerated people with drug convictions (not automatically released under legalization), indigenous/traditional users whose practices may not fit commercial licensing, low-income users priced out by regulatory floors. They are structurally excluded from the licensing regime design.
% DISAPPEARANCE_RATIONALE: If the regulatory framework vanished overnight, the legal market would collapse into either unregulated commercial sales (no quality control, no youth access limits, no tax capture) or revert to prohibition enforcement. Both outcomes rearrange the world: the former creates new harms, the latter restores the carceral apparatus. The constraint's existence is what holds the specific legalization arrangement in place.
% FOUNDING_PROBLEM: Prohibition created a violent illicit market, poisoned drug supply, mass incarceration for possession, and zero public health visibility into use patterns — while failing to reduce availability or use.
% FOUNDING_PROBLEM_CORROBORATION: Legalization proponents (state agencies, industry, some public health officials) attest the founding problem is substantially solved: illicit market displaced, product safety achieved, arrests plummeted. Critics (harm reduction advocates, some economists, communities impacted by commercialization) attest the founding problem persists in new form: commercial incentives drive increased use, high-risk users remain underserved, regulatory capture entrenches. Corroboration from outside beneficiaries: independent legislative audits in WA/CO/CA, academic studies (Caulkins, Kilmer, Pacula), and international bodies (EMCDDA, WHO) document both displacement of illicit markets and emergence of commercialization harms.
narrative_ontology:disappearance_verdict(substance_control_authority__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__legalization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(substance_control_authority__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__legalization_reading, 0.32, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__legalization_reading_tests).
:- end_tests(substance_control_authority__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.32) reflects the state's cut (taxes, fees) and compliance costs passed to consumers — real but bounded by competitive displacement of illicit market. Suppression (0.41) is moderate: the constraint suppresses illicit market participation and unregulated sales, but legal market access is broadly available to adults. Theater ratio (0.22) is low-moderate: regulatory functions (testing, labeling, tracking) are genuinely performed, but some rules (purchase limits, advertising bans) serve performative 'tough on drugs' signaling. Accessibility collapse (0.48) is partial: legal market creates new access for many, but barriers (price, ID, limits) exclude high-risk users. Resistance (0.55) is significant: from illicit market holdouts, prohibitionist political forces, and commercial operators resisting stricter rules.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter (state) experiences this as coordination: it built a system that solves prohibition's failures. Legal operators experience it as coordination-with-rent: they get a regulated monopoly. Consumer users experience it as net-beneficial coordination. Illicit operators experience it as suppression of their livelihood. High-risk users experience it as a new form of exclusion: legal market barriers replace prohibition's barriers. The engine will compute per-seat types from these structural positions — the claimed tangled_rope reflects the aggregate structure: genuine coordination (safety, revenue, displacement) with asymmetric extraction (state cut, operator rents, high-risk user exclusion) requiring active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   State agencies are agenda_setters (d ~0.15): they extract fees but bear regulatory costs; net slight beneficiary. Legal operators are beneficiaries (d ~0.2): they gain legal market access and barriers to entry; compliance costs are the price of monopoly rents. Consumer users are near-symmetric beneficiaries/payers (d ~0.5): gain safety/legality, pay tax-inclusive prices; mobile exit. Tax authorities and public health agencies are analytical/institutional beneficiaries (d ~0.1): collect revenue/data without operational burden. Illicit market participants are targets (d ~0.85): lose market, face enforcement; constrained exit. High-risk users are trapped victims (d ~0.95): excluded by design barriers, dependent on supply, no viable exit. Third-party public is analytical beneficiary (d ~0.1): gains public goods. Harm reduction advocates are observers (d ~0.5): analytical seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prohibition's failures) is contested as live/dead. If dead (illicit market displaced, safety achieved), the constraint risks mandatrophy: regulatory apparatus persists, commercial interests entrench, high-risk users remain underserved. The theater ratio rising (0.1→0.22) suggests performative maintenance growing. The constraint is not yet a piton — active enforcement still suppresses illicit markets and maintains quality standards — but the drift vector points toward institutional inertia protecting the regulatory regime itself rather than the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_structure,
    'This constraint is one reading (legalization_reading) of the contested kernel substance_control_authority. What structural elements do the sibling readings (prohibition_reading, harm_reduction_reading) share vs. differ on, and how does that affect classification stability?',
    'Author the sibling readings as separate constraint stories with their own ε, beneficiaries, victims, and metrics. Compare the computed per-seat classifications across the family. The kernel''s structural core is the set of elements invariant across readings; the reading-specific elements are where classification diverges.',
    'If sibling readings produce the same constraint type despite different beneficiary/victim structures, the kernel carries a structural invariant. If they diverge (e.g., prohibition_reading = snare, harm_reduction_reading = rope, legalization_reading = tangled_rope), the kernel is a site of genuine contestation where the same authority claim produces different structural realities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_structure, conceptual, 'Kernel-reading decomposition: how the three readings of substance_control_authority structurally relate and whether the kernel itself has a stable classification.').

omega_variable(
    commercialization_extraction_boundary,
    'At what point does the legal market''s commercial incentive structure (marketing, product innovation, price competition) convert the constraint from coordination-with-extraction into extraction-with-coordination-cover?',
    'Longitudinal tracking of: marketing restrictions enforcement, product potency trends, use prevalence by risk tier, regulatory capture indicators (revolving door, lobbying spend vs. public health budget), and high-risk user access metrics. A sustained pattern of rising use volume concentrated in high-risk tiers, weakening marketing rules, and regulatory capture would shift the boundary.',
    'If commercialization crosses the boundary, the constraint reclassifies from tangled_rope toward snare: the coordination function (safety, displacement) becomes subordinate to the extraction function (commercial profit, state revenue, regulatory self-preservation). The claimed_type would need revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercialization_extraction_boundary, empirical, 'Whether the legalization reading''s commercial regulation mechanism inevitably drifts toward extractive commercialization that undermines its own coordination claims.').

omega_variable(
    high_risk_user_exclusion_mechanism,
    'Are high-risk users'' access barriers (price floors, purchase limits, ID requirements, stigma) an inevitable feature of any regulated market, or a design choice that could be modified without collapsing the regulatory framework?',
    'Compare jurisdictions with different regulatory designs: low-barrier models (Uruguay pharmacy sales, DC gifting model, safe supply programs) vs. high-barrier commercial models (WA, CO, CA). Track high-risk user outcomes (overdose, retention in care, illicit market use) across designs.',
    'If barriers are design choices, the victim status of high-risk users is contingent — the constraint could be reformed to reduce extraction on this group without abandoning legalization. If barriers are structurally necessary (e.g., any commercial market must price out dependent users to be viable), the victim set is intrinsic to the legalization reading''s architecture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(high_risk_user_exclusion_mechanism, conceptual, 'Whether high-risk user exclusion is a necessary structural feature of commercial regulation or a modifiable policy choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__legalization_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__legalization_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(subs_tr_t3, substance_control_authority__legalization_reading, theater_ratio, 3, 0.14).
narrative_ontology:measurement(subs_tr_t6, substance_control_authority__legalization_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(subs_tr_t9, substance_control_authority__legalization_reading, theater_ratio, 9, 0.2).
narrative_ontology:measurement(subs_tr_t12, substance_control_authority__legalization_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(subs_tr_t15, substance_control_authority__legalization_reading, theater_ratio, 15, 0.22).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__legalization_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(subs_be_t3, substance_control_authority__legalization_reading, base_extractiveness, 3, 0.24).
narrative_ontology:measurement(subs_be_t6, substance_control_authority__legalization_reading, base_extractiveness, 6, 0.28).
narrative_ontology:measurement(subs_be_t9, substance_control_authority__legalization_reading, base_extractiveness, 9, 0.3).
narrative_ontology:measurement(subs_be_t12, substance_control_authority__legalization_reading, base_extractiveness, 12, 0.31).
narrative_ontology:measurement(subs_be_t15, substance_control_authority__legalization_reading, base_extractiveness, 15, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__legalization_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(subs_su_t3, substance_control_authority__legalization_reading, suppression_requirement, 3, 0.48).
narrative_ontology:measurement(subs_su_t6, substance_control_authority__legalization_reading, suppression_requirement, 6, 0.44).
narrative_ontology:measurement(subs_su_t9, substance_control_authority__legalization_reading, suppression_requirement, 9, 0.42).
narrative_ontology:measurement(subs_su_t12, substance_control_authority__legalization_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement(subs_su_t15, substance_control_authority__legalization_reading, suppression_requirement, 15, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__legalization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_authority__legalization_reading, 0.18).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is the legalization_reading of the substance_control_authority kernel. The prohibition_reading criminalizes use/possession to protect third parties (snare structure). The harm_reduction_reading accepts use and minimizes harm via public health (rope/scaffold structure). All three claim the same state authority but instantiate different constraints with different beneficiary/victim structures, extraction profiles, and enforcement logics. The legalization reading uniquely creates a commercial market as the regulatory mechanism, producing the tangled_rope structure: genuine coordination (safety, displacement, revenue) with asymmetric extraction (commercial rents, state cut, high-risk exclusion) requiring active enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__legalization_reading, institutional, 0.15).
constraint_indexing:directionality_override(substance_control_authority__legalization_reading, organized, 0.2).
constraint_indexing:directionality_override(substance_control_authority__legalization_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
