% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__expansive_universalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__expansive_universalist, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: equality_clause_scope__expansive_universalist
 *   human_readable: Equality Clause — Expansive Universalist Reading
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested 'equality clause
 *   scope' kernel: the expansive universalist reading, under which
 *   constitutional equality language is self-evidently universal in
 *   application, and every historical instance of exclusion (chattel slavery,
 *   women's disenfranchisement, property qualifications, racial segregation)
 *   is read as hypocrisy or betrayal of the founding principle rather than as
 *   evidence of the clause's true original scope. This reading sets a
 *   comparatively low threshold for judicial expansion of coverage: courts
 *   need not wait for constitutional amendment to recognize a previously
 *   excluded group, because the text is read as having always covered them in
 *   principle. Two sibling readings exist as separate constraint stories:
 *   restrictive_originalist (which reads the clause as scoped to propertied
 *   white males within an 18th-century social-contract framework, treating
 *   that scope as the clause's actual original meaning) and
 *   progressive_textualist (which agrees the text embeds a genuine equality
 *   principle but insists expansion must proceed through democratic
 *   amendment, not judicial reinterpretation). This story's epsilon,
 *   beneficiary/victim structure, and classification are authored
 *   independently of those siblings' — they are not measurement-parameter
 *   variants of one constraint but three structurally distinct constraints
 *   sharing a kernel text.
 *
 * KEY AGENTS:
 *   - historically_excluded_groups: primary beneficiary (powerless/trapped) — gains standing under the universalist premise
 *   - rights_claimants_before_courts: beneficiary and cost-bearer (moderate/constrained) — invests in litigation strategy premised on this reading
 *   - judiciary_expansive_wing: agenda-setter (institutional/arbitrage) — administers and extends the doctrine through rulings
 *   - beneficiaries_of_historical_exclusion_regimes: payer (powerful/constrained) — loses relative advantage as coverage expands
 *   - originalist_legal_traditionalists: payer and excluded voice (organized/constrained) — objects to the reading's legitimacy but does not control outcomes
 *   - constitutional_historians: analytical observer — assesses the historical-hypocrisy claim against the ratification record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, 0.28).
domain_priors:suppression_score(equality_clause_scope__expansive_universalist, 0.32).
domain_priors:theater_ratio(equality_clause_scope__expansive_universalist, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, extractiveness, 0.28).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__expansive_universalist, rope).
narrative_ontology:human_readable(equality_clause_scope__expansive_universalist, "Equality Clause — Expansive Universalist Reading").
narrative_ontology:topic_domain(equality_clause_scope__expansive_universalist, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(equality_clause_scope__expansive_universalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__expansive_universalist, '1feea69b-7680-42fd-b06a-cd5f0b13c97d').
narrative_ontology:cs_kernel_codification('1feea69b-7680-42fd-b06a-cd5f0b13c97d', fixed_text).
narrative_ontology:cs_authority_grounding('1feea69b-7680-42fd-b06a-cd5f0b13c97d', lineage).
narrative_ontology:cs_interpretation_layer_present('1feea69b-7680-42fd-b06a-cd5f0b13c97d').
narrative_ontology:cs_reading_relation('1feea69b-7680-42fd-b06a-cd5f0b13c97d', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('1feea69b-7680-42fd-b06a-cd5f0b13c97d', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('1feea69b-7680-42fd-b06a-cd5f0b13c97d', foundational, equality_principle_is_universal_and_self_evident).
narrative_ontology:cs_axiom_status(equality_principle_is_universal_and_self_evident, holdable).
narrative_ontology:cs_axiom_grounding('1feea69b-7680-42fd-b06a-cd5f0b13c97d', equality_principle_is_universal_and_self_evident, deontological).
narrative_ontology:cs_axiom('1feea69b-7680-42fd-b06a-cd5f0b13c97d', foundational, historical_exclusion_is_hypocrisy_not_precedent).
narrative_ontology:cs_axiom_status(historical_exclusion_is_hypocrisy_not_precedent, holdable).
narrative_ontology:cs_axiom_grounding('1feea69b-7680-42fd-b06a-cd5f0b13c97d', historical_exclusion_is_hypocrisy_not_precedent, conventional).
narrative_ontology:cs_axiom('1feea69b-7680-42fd-b06a-cd5f0b13c97d', secondary, judicial_reinterpretation_is_legitimate_expansion_mechanism).
narrative_ontology:cs_axiom_status(judicial_reinterpretation_is_legitimate_expansion_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('1feea69b-7680-42fd-b06a-cd5f0b13c97d', judicial_reinterpretation_is_legitimate_expansion_mechanism, instrumental).
narrative_ontology:cs_reference_frame('1feea69b-7680-42fd-b06a-cd5f0b13c97d', universal_natural_rights_principle).
narrative_ontology:cs_drift_state('1feea69b-7680-42fd-b06a-cd5f0b13c97d', contemporary_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('1feea69b-7680-42fd-b06a-cd5f0b13c97d', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__expansive_universalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, historically_excluded_groups).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, rights_claimants_before_courts).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, civil_rights_movements).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, beneficiaries_of_historical_exclusion_regimes).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, originalist_legal_traditionalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, rights_claimants_before_courts).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, self_evident_universal_equality_doctrine).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, living_constitution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups denied full legal personhood or equal treatment under earlier applications of the same constitutional text (enslaved people, women, non-property-holders, racial minorities, and their descendants) gain standing to claim the equality guarantee applies to them without needing a new constitutional amendment. They cannot exit the polity; their only route to inclusion is reinterpretation of the existing text.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, historically_excluded_groups, beneficiary,
    powerless, generational, trapped, national).

% Litigants and advocacy organizations bring equal-protection claims relying on the universalist reading to expand coverage (marriage equality, disability rights, gender discrimination). They invest years and resources in litigation whose success depends entirely on courts accepting that the clause's plain meaning already includes them, with no guarantee of a favorable ruling.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, rights_claimants_before_courts, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__expansive_universalist, rights_claimants_before_courts, payer).

% Organized political and legal movements build strategy around the premise that equality was always the true meaning of the text and historical exclusion was betrayal of that meaning, not a competing legitimate reading. This framing gives their claims moral urgency and constitutional grounding simultaneously, but they depend on judiciaries willing to adopt the universalist premise.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, civil_rights_movements, beneficiary,
    organized, generational, constrained, national).

% Judges and justices who adopt the expansive universalist premise treat historical exclusions as errors to be corrected through interpretation rather than as binding evidence of original scope. They administer the doctrine by issuing rulings that extend coverage, setting precedent that lowers the threshold for future expansion claims.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, judiciary_expansive_wing, agenda_setter,
    institutional, generational, arbitrage, national).

% Groups and institutions whose prior advantage depended on a narrower reading of who counted as equal (e.g. exclusive access to suffrage, property, or office) lose relative position as coverage expands. They cannot easily exit the jurisdiction and increasingly must compete or share status with newly included groups.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, beneficiaries_of_historical_exclusion_regimes, payer,
    powerful, biographical, constrained, national).

% Legal scholars and jurists committed to reading the clause according to its drafters' understood scope experience the universalist reading as illegitimate judicial rewriting of the text without democratic amendment. Their objection is structurally present in dissenting opinions and academic literature but does not control outcomes when the expansive wing holds judicial power.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, originalist_legal_traditionalists, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__expansive_universalist, originalist_legal_traditionalists, excluded).

% Those who believe scope expansion should occur through Article-V-style amendment rather than judicial reinterpretation are sidelined by this reading's low threshold for court-driven expansion — their preferred procedural pathway is bypassed whenever courts find the universal meaning already present in the existing text.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, democratic_amendment_process_advocates, excluded,
    organized, generational, constrained, national).

% Study the drafting record, ratification debates, and subsequent application history to assess whether the universalist premise reflects an original but betrayed meaning or a later constructed reading projected backward onto the text.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single stable textual anchor — 'equality' — that diverse claimant groups across two centuries can invoke without requiring a new constitutional amendment for each newly recognized class, coordinating rights expansion around one interpretive doctrine rather than fragmenting it into group-specific amendments.
% TRANSFER_FUNCTION: Moves legal standing, political voice, and material protections from groups that held exclusive advantage under narrower historical applications toward groups previously excluded, mediated through judicial recognition rather than legislative reallocation.
% ABSENT_VOICES: Originalist jurists and democratic-amendment-process advocates object that the reading substitutes judicial will for the constitutionally specified amendment procedure; their objections appear in dissents and scholarship but do not carry decisive weight once the expansive judicial wing holds a majority.
% DISAPPEARANCE_RATIONALE: If the universalist reading were abandoned, previously excluded groups would lose the doctrinal basis for many existing protections built up through decades of case law (world_rearranges from their seat); originalist traditionalists would say the world merely returns to its textually correct baseline (world_unchanged from their seat) — the parties dispute which baseline is the true one, which is the kernel contest itself.
% FOUNDING_PROBLEM: The founding problem, as this reading states it, is that the constitutional guarantee of equality was written in universal terms but applied hypocritically and narrowly at the time of drafting and for generations after — the arrangement exists to correct that gap between stated principle and practiced exclusion.
% FOUNDING_PROBLEM_CORROBORATION: Historians outside the direct beneficiary groups (constitutional historians studying ratification debates) corroborate that the text's language was broader than its contemporaneous application, supporting the hypocrisy-correction framing; however, originalist scholars — also outside the beneficiary groups but adverse to this reading — dispute that the drafters intended or understood the universal application, so corroboration for the founding-problem framing itself is contested rather than unanimous.
narrative_ontology:disappearance_verdict(equality_clause_scope__expansive_universalist, contested).
narrative_ontology:founding_problem_status(equality_clause_scope__expansive_universalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__expansive_universalist, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equality_clause_scope__expansive_universalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__expansive_universalist, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__expansive_universalist_tests).
:- end_tests(equality_clause_scope__expansive_universalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.28) and rising slowly: the reading itself does not extract resources so much as redistribute standing and legal recognition, and even that redistribution has decelerated as major expansion episodes (1868, 1954, 1980s, 2003+) become rarer and more incremental. Suppression starts moderate (0.55 at 1868, reflecting the active suppression required to maintain narrow readings against the newly ratified universal text) and falls steadily as the universalist reading becomes more institutionally entrenched and requires less active enforcement against contrary readings. Theater ratio starts high (0.4) — early invocations of 'equality' alongside contemporaneous exclusionary practice were substantially performative — and falls as the doctrine's application becomes more substantively realized in case law. Accessibility collapse (0.45) is moderate: alternative readings (originalist, textualist) remain live and contested, they have not collapsed, which is itself part of what makes this a genuine kernel contest rather than a settled mountain. Resistance (0.58) is correspondingly substantial: originalist jurisprudence is an organized, persistent counter-tradition, not a fringe position.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically excluded groups and rights claimants sit near the beneficiary end: the universalist reading is the doctrinal vehicle through which they gain recognition they lacked under narrower historical applications. The judiciary's expansive wing administers the doctrine and enjoys the most institutional latitude (arbitrage-grade exit — they can select which cases to hear and how broadly to rule). Beneficiaries of the old exclusion regimes and originalist traditionalists sit toward the target end: their prior relative advantage, or their preferred interpretive methodology, is what the expansive reading displaces. Their exit is constrained, not trapped — political and academic advocacy remains open to them even though it is not currently controlling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (textual universalism betrayed by exclusionary practice) is authored as still live rather than resolved, because contested groups and contested applications continue to arise (LGBTQ+ rights, disability rights, non-citizen protections) under the same doctrinal logic. This prevents the classification from either (a) treating the reading as pure historical artifact whose work is done, or (b) treating every new expansion claim as automatically vindicated — the mismatch check (status=live + verdict=contested) signals an active, unresolved kernel contest rather than a captured or zombie doctrine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_vs_backward_projection,
    'Did the drafters and ratifiers of the equality clause understand its language to already encompass currently-excluded groups, or is the universalist reading a normatively appealing but historically inaccurate projection of later values backward onto the text?',
    'Close historical analysis of drafting debates, contemporaneous commentary, and immediate post-ratification application; comparison against the restrictive_originalist reading''s claimed evidentiary basis.',
    'If the drafters'' understood scope was genuinely narrow, the universalist reading is a constructed doctrine wearing the original text as cover — closer to a false-summit dynamic where ''the text always meant this'' benefits current claimants by obscuring that the doctrine is itself a substantive innovation. If the broader principle was genuinely present but inconsistently applied, the hypocrisy-correction framing holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_vs_backward_projection, conceptual, 'Whether the universalist premise reflects genuine original meaning or retrospective doctrinal construction.').

omega_variable(
    judicial_legitimacy_threshold,
    'Is judicial recognition of expanded equality coverage a legitimate exercise of interpretive authority, or does it improperly substitute courts for the constitutionally specified amendment process?',
    'Comparative analysis of amendment-based versus interpretation-based rights expansions across jurisdictions and their subsequent stability/reversal rates.',
    'If interpretation-based expansion proves less durable or more contested over time than amendment-based expansion, it would support the progressive_textualist sibling''s procedural objection to this reading''s low legitimacy threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_legitimacy_threshold, preference, 'Whether judicial reinterpretation is a legitimate mechanism for expanding constitutional coverage.').

omega_variable(
    kernel_framing_underdetermination,
    'Is ''equality clause scope'' better modeled as one kernel with three readings, or does the restrictive_originalist reading actually describe a different historical constraint entirely (the 18th-century social contract) that has since been superseded rather than merely reinterpreted?',
    'Track whether restrictive_originalist and expansive_universalist ever coexist as live options within a single judicial framework simultaneously, or whether one fully displaces the other at each historical moment (forecloses vs. coexists_with test).',
    'If the readings never genuinely coexist within one framework at the same time, the forecloses relation is the more accurate structural characterization and the kernel model should reflect sequential displacement rather than standing contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Alternative framing: sequential displacement of readings versus standing multi-party contest over one persistent kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__expansive_universalist, 1868, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1868, equality_clause_scope__expansive_universalist, theater_ratio, 1868, 0.4).
narrative_ontology:measurement(equa_tr_t1900, equality_clause_scope__expansive_universalist, theater_ratio, 1900, 0.38).
narrative_ontology:measurement(equa_tr_t1954, equality_clause_scope__expansive_universalist, theater_ratio, 1954, 0.28).
narrative_ontology:measurement(equa_tr_t1980, equality_clause_scope__expansive_universalist, theater_ratio, 1980, 0.24).
narrative_ontology:measurement(equa_tr_t2003, equality_clause_scope__expansive_universalist, theater_ratio, 2003, 0.23).
narrative_ontology:measurement(equa_tr_t2026, equality_clause_scope__expansive_universalist, theater_ratio, 2026, 0.22).

% Extraction over time
narrative_ontology:measurement(equa_be_t1868, equality_clause_scope__expansive_universalist, base_extractiveness, 1868, 0.12).
narrative_ontology:measurement(equa_be_t1900, equality_clause_scope__expansive_universalist, base_extractiveness, 1900, 0.15).
narrative_ontology:measurement(equa_be_t1954, equality_clause_scope__expansive_universalist, base_extractiveness, 1954, 0.2).
narrative_ontology:measurement(equa_be_t1980, equality_clause_scope__expansive_universalist, base_extractiveness, 1980, 0.24).
narrative_ontology:measurement(equa_be_t2003, equality_clause_scope__expansive_universalist, base_extractiveness, 2003, 0.26).
narrative_ontology:measurement(equa_be_t2026, equality_clause_scope__expansive_universalist, base_extractiveness, 2026, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1868, equality_clause_scope__expansive_universalist, suppression_requirement, 1868, 0.55).
narrative_ontology:measurement(equa_su_t1900, equality_clause_scope__expansive_universalist, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(equa_su_t1954, equality_clause_scope__expansive_universalist, suppression_requirement, 1954, 0.42).
narrative_ontology:measurement(equa_su_t1980, equality_clause_scope__expansive_universalist, suppression_requirement, 1980, 0.36).
narrative_ontology:measurement(equa_su_t2003, equality_clause_scope__expansive_universalist, suppression_requirement, 2003, 0.33).
narrative_ontology:measurement(equa_su_t2026, equality_clause_scope__expansive_universalist, suppression_requirement, 2026, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__progressive_textualist).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the equality_clause_scope kernel (expansive_universalist, restrictive_originalist, progressive_textualist). Each reading is authored as its own constraint with its own epsilon and beneficiary/victim structure, per the epsilon-invariance decomposition principle. expansive_universalist and restrictive_originalist stand in a forecloses relationship at the level of core premise (universal original scope vs. propertied-white-male original scope cannot both be true of the same drafting intent); expansive_universalist and progressive_textualist stand in a coexists_with relationship (both affirm a genuine universal equality principle in the text; they disagree only on the legitimate mechanism — judicial interpretation vs. democratic amendment — for realizing expanded coverage).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
