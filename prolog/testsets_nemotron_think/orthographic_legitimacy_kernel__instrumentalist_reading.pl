% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__instrumentalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__instrumentalist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__instrumentalist_reading
 *   human_readable: Orthographic Legitimacy via Literacy Maximization (Instrumentalist Reading)
 *   domain: political/linguistic/state_formation
 *
 * SUMMARY:
 *   The instrumentalist reading of orthographic legitimacy frames script
 *   reform as a pragmatic intervention: replace the Ottoman Arabic script
 *   with a Latin-based phonemic alphabet to maximize literacy rates and
 *   administrative efficiency. This reading powered the 1928 Turkish script
 *   reform and its imitators. It claims rope-like coordination (a genuine
 *   collective-action problem solved with minimal coercive overhead), but the
 *   structural data reveals asymmetric extraction: the Arabic-literate elite
 *   lose their professional capital without compensation, and the state
 *   actively suppresses the old script. The constraint is therefore a tangled
 *   rope — coordination function present, but enforced extraction layered
 *   atop it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, 0.45).
domain_priors:suppression_score(orthographic_legitimacy_kernel__instrumentalist_reading, 0.65).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__instrumentalist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__instrumentalist_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__instrumentalist_reading, "Orthographic Legitimacy via Literacy Maximization (Instrumentalist Reading)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__instrumentalist_reading, "political/linguistic/state_formation").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__instrumentalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__instrumentalist_reading, '039215d5-1c0f-4cb5-ad1b-c32905f656f1').
narrative_ontology:cs_kernel_codification('039215d5-1c0f-4cb5-ad1b-c32905f656f1', formalized).
narrative_ontology:cs_authority_grounding('039215d5-1c0f-4cb5-ad1b-c32905f656f1', extraction).
narrative_ontology:cs_interpretation_layer_present('039215d5-1c0f-4cb5-ad1b-c32905f656f1').
narrative_ontology:cs_reading_relation('039215d5-1c0f-4cb5-ad1b-c32905f656f1', orthographic_legitimacy_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('039215d5-1c0f-4cb5-ad1b-c32905f656f1', orthographic_legitimacy_kernel__modernist_reading, coexists_with).
narrative_ontology:cs_axiom('039215d5-1c0f-4cb5-ad1b-c32905f656f1', foundational, literacy_maximization_justifies_script_change).
narrative_ontology:cs_axiom_status(literacy_maximization_justifies_script_change, holdable).
narrative_ontology:cs_axiom_grounding('039215d5-1c0f-4cb5-ad1b-c32905f656f1', literacy_maximization_justifies_script_change, empirically_contingent).
narrative_ontology:cs_axiom('039215d5-1c0f-4cb5-ad1b-c32905f656f1', secondary, administrative_efficiency_requires_script_standardization).
narrative_ontology:cs_axiom_status(administrative_efficiency_requires_script_standardization, holdable).
narrative_ontology:cs_axiom_grounding('039215d5-1c0f-4cb5-ad1b-c32905f656f1', administrative_efficiency_requires_script_standardization, instrumental).
narrative_ontology:cs_reference_frame('039215d5-1c0f-4cb5-ad1b-c32905f656f1', pragmatic_literacy_framework).
narrative_ontology:cs_drift_state('039215d5-1c0f-4cb5-ad1b-c32905f656f1', contemporary_digital_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('039215d5-1c0f-4cb5-ad1b-c32905f656f1', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_elite).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, state_administration).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__instrumentalist_reading, literacy_rate_maximization_justifies_script_reform).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__instrumentalist_reading, administrative_efficiency_requires_script_standardization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains access to literacy and administrative participation through a simplified, phonemic script that can be learned in months rather than years. Their children attend schools where the new script is the sole medium. Exit means emigrating or opting out of state education, which is costly but possible.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population, beneficiary,
    organized, generational, mobile, national).

% Ottoman-era bureaucrats, ulema, and intellectuals whose professional capital (reading/writing Ottoman Turkish in Arabic script) is devalued overnight. They cannot easily retrain; their access to historical texts, religious authority, and administrative positions erodes. Some emigrate; most are marginalized within the new system.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_elite, payer,
    powerful, biographical, constrained, national).

% Enacts and enforces the script reform by law (1928 Law on the Adoption and Application of Turkish Letters). Controls education, publishing, and official communication. Gains a legible, standardized citizenry and a break from Ottoman administrative opacity. The reform's irreversibility becomes a source of regime legitimacy.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, state_administration, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__instrumentalist_reading, state_administration, beneficiary).

% Lose control over the script of revelation and religious education. The Quran in Arabic script becomes inaccessible to the new generation without mediation. They are excluded from the reform debate; their objections are framed as reactionary. Their authority rests on a script the state has declared obsolete.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, religious_authorities, excluded,
    organized, generational, identity_locked, national).

% Scholars of comparative script reform, literacy campaigns, and nation-building. They evaluate the reform's literacy statistics, its role as a model for other post-colonial states, and the trade-offs between accessibility and cultural continuity. They do not bear the reform's costs or collect its gains.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, international_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizing the script to a phonemic, Latin-based alphabet solves the coordination problem of mass literacy acquisition and administrative communication across a linguistically diverse territory. A single, easily learned script replaces a complex, Arabic-based system that required years of specialized training.
% TRANSFER_FUNCTION: Moves literacy access and administrative participation from the Arabic-literate elite (who held a monopoly on the complex script) to the broader population. The elite's specialized script knowledge is devalued; the state captures the gains of a standardized, legible citizenry. The transfer is justified by aggregate literacy statistics, not by compensation to the displaced elite.
% ABSENT_VOICES: The Arabic-literate elite (Ottoman bureaucrats, ulema, poets, calligraphers) and religious traditionalists who view the script as a sacred trust. They are structurally excluded from the reform's legislative process and its educational implementation. Their objection — that the reform severs the population from its textual heritage — is dismissed as irrational nostalgia.
% DISAPPEARANCE_RATIONALE: If the script reform and its enforcement vanished overnight, the education system would lose its sole medium of instruction, official documents would become unreadable to the current population, and the state would lose its primary instrument of administrative standardization. The Turkish language would face a script vacuum — neither the new generation nor the bureaucracy can operate in Arabic script.
% FOUNDING_PROBLEM: The Ottoman Arabic script was poorly suited to Turkish phonology (vowel harmony, eight vowels vs. three Arabic letters), requiring years of memorization and producing literacy rates below 10%. The state needed a script that could be learned quickly by conscripts, civil servants, and schoolchildren to build a modern administrative apparatus.
% FOUNDING_PROBLEM_CORROBORATION: Early republican literacy statistics (1927: 10.6% → 1950: 32.7%) are cited by the state and secular historians as proof the founding problem was solved. Traditionalist scholars and Ottomanists contest this, arguing that literacy gains came from expanded schooling, not script change alone, and that the cultural severance created new problems (inaccessibility of pre-1928 archives, religious illiteracy). No neutral arbiter reconciles these accounts.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__instrumentalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__instrumentalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__instrumentalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).
:- end_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.45) is moderate: the reform delivers real coordination gains (literacy rose from ~10% to >30% in two decades) but extracts from the elite whose script capital is expropriated. Suppression (0.65) is high initially (ban on Arabic script, penalization of its use) but decays as the new script becomes universal; it rises again in the digital era as the state polices script purity against 'Arabic letter' intrusion. Theater ratio (0.30) reflects genuine literacy gains mixed with performative nationalism (script as symbol of Westernization). Accessibility collapse (0.70) is high: the new generation cannot read pre-1928 texts without specialized training. Resistance (0.50) peaks at reform (elite petitions, Kurdish revolts partly script-linked) then declines as the old elite dies out.
 *
 * PERSPECTIVAL GAP:
 *   From the state_administration seat, the constraint computes as rope (coordination delivered, extraction justified). From the arabic_literate_elite seat, it computes as snare (extraction without consent, suppression of alternatives). From the newly_literate_population seat, it computes as rope (net benefit, exit available). The engine computes this divergence from the structural data; the instrumentalist reading's claim of pure coordination is the gap.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_administration is the agenda_setter and secondary beneficiary (d ≈ 0.15): it writes the rules and captures administrative legibility. The newly_literate_population are primary beneficiaries (d ≈ 0.25): they gain literacy access but bear indirect costs (cultural severance). The arabic_literate_elite are payers (d ≈ 0.85): their human capital is devalued, exit is constrained (retraining at mid-career, emigration costs). Religious_authorities are excluded and identity-locked (d ≈ 0.90): their authority is script-dependent and the reform severs that link. International_observers are analytical (d = 0.50).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (low literacy, administrative opacity) was live in 1928. By 2000, literacy exceeded 85% and administrative standardization was achieved — the coordination function is saturated. Yet the constraint persists with active enforcement (script purity laws, education monopoly). The mandatrophy is unresolved: the arrangement has outlived its founding problem but the state derives legitimacy from its irreversibility. This is the extraction component of the tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does the instrumentalist_reading foreclose, coexist with, or influence the continuity_reading and modernist_reading within a single commitment framework?',
    'Analyze whether a state can simultaneously hold ''script reform is justified by literacy statistics'' AND ''script reform is justified by civilizational rupture'' AND ''script reform is unjustified because it severs tradition'' as coherent positions. Historical evidence: all three rhetorics appeared in Turkish discourse; different factions held different ones.',
    'If forecloses: this reading''s axiomatic premise (literacy statistics are the sole legitimacy ground) logically rules out the others. If coexists_with: all three remain live positions held by different parties. If influences: this reading''s success changes the legitimacy conditions for the others (e.g., literacy gains make continuity_reading harder to sustain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural relationship between this kernel reading and its siblings').

omega_variable(
    literacy_causality_ambiguity,
    'How much of the measured literacy gain is attributable to script change versus expanded schooling, and does the ambiguity affect ε?',
    'Natural experiment comparison: Turkish reform vs. other Muslim-majority states that expanded schooling without script change (e.g., Iran, Egypt). Disaggregate script effect from schooling effect using cohort analysis.',
    'If script change contributed little to literacy gains, the coordination function is overstated and ε rises (more extraction, less coordination). If script change was decisive, ε falls toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literacy_causality_ambiguity, empirical, 'Causal attribution of literacy gains to script reform versus schooling expansion').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal ban, educational monopoly) or internalized (population self-censors Arabic script, views it as backward)?',
    'Post-reform suppression trajectory: if Arabic script use persists in private/religious contexts despite legal ban, suppression is partly internalized. Survey data on attitudes toward Arabic script over generations.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint persists without active enforcement because the population has adopted the state''s valuation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in script reform').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__instrumentalist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_leg_inst_tr_t0, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(orth_leg_inst_tr_t20, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(orth_leg_inst_tr_t40, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(orth_leg_inst_tr_t60, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(orth_leg_inst_tr_t80, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 80, 0.3).
narrative_ontology:measurement(orth_leg_inst_tr_t100, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(orth_leg_inst_be_t0, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(orth_leg_inst_be_t20, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(orth_leg_inst_be_t40, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(orth_leg_inst_be_t60, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(orth_leg_inst_be_t80, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(orth_leg_inst_be_t100, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(orth_leg_inst_su_t0, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(orth_leg_inst_su_t20, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(orth_leg_inst_su_t40, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(orth_leg_inst_su_t60, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(orth_leg_inst_su_t80, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(orth_leg_inst_su_t100, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__instrumentalist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(orthographic_legitimacy_kernel__instrumentalist_reading, 0.02).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__modernist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the orthographic_legitimacy_kernel. The instrumentalist_reading claims legitimacy derives from literacy maximization and administrative efficiency (moderate ε, tangled_rope). The continuity_reading claims legitimacy derives from preserving tradition (low ε, mountain-claimed but FSM candidate). The modernist_reading claims legitimacy derives from Western alignment (moderate-high ε, snare/tangled_rope). All three share the same referent (script reform) but author different ε and different victim/beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_legitimacy_kernel__instrumentalist_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
