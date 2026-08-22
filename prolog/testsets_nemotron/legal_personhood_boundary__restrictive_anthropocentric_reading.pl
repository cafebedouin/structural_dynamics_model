% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__restrictive_anthropocentric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__restrictive_anthropocentric_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: legal_personhood_boundary__restrictive_anthropocentric_reading
 *   human_readable: Legal Personhood Restricted to Born Humans with Cognitive Capacity
 *   domain: legal/philosophical/constitutional
 *
 * SUMMARY:
 *   This constraint instantiates the restrictive anthropocentric reading of
 *   the legal personhood boundary kernel. It limits constitutional and legal
 *   personhood to born human beings who possess or once possessed cognitive
 *   capacity (rationality, self-awareness, sentience). This reading excludes
 *   fetuses at all gestational stages, permanently cognitively impaired
 *   humans, nonhuman animals, ecosystems, and artificial intelligence systems
 *   from the victim set — they cannot be rights-bearers under this
 *   constraint. The coordination function is genuine: it provides a clear,
 *   administrable threshold for legal subjecthood that avoids metaphysical
 *   disputes about ensoulment or potentiality, and it protects pregnant
 *   persons' autonomy from state coercion. The extraction function is
 *   asymmetric: the excluded entities bear the full cost of their exclusion
 *   (denial of legal standing, vulnerability to instrumental use) while the
 *   beneficiaries (pregnant persons, researchers, development interests) gain
 *   concrete liberties and economic advantages. Active enforcement is
 *   required through judicial precedent, statutory interpretation, and
 *   constitutional doctrine to maintain the boundary against developmental
 *   and functional challengers.
 *
 * KEY AGENTS:
 *   - pregnant_persons: Primary beneficiary (institutional/mobile) — gains reproductive autonomy and bodily integrity protections
 *   - fetuses: Primary victim (powerless/trapped) — denied legal standing and protection from abortion
 *   - severely_cognitively_impaired_humans: Victim (powerless/trapped) — excluded from full personhood despite human species membership
 *   - nonhuman_animals: Victim (powerless/trapped) — denied rights despite cognitive complexity
 *   - ecosystems: Victim (powerless/trapped) — excluded from legal standing for environmental protection
 *   - artificial_intelligence_systems: Victim (powerless/trapped) — denied personhood regardless of functional capacity
 *   - reproductive_healthcare_providers: Beneficiary (organized/constrained) — legal clarity and protection for abortion care
 *   - biomedical_researchers: Beneficiary (organized/constrained) — access to fetal tissue and embryo research
 *   - environmental_development_interests: Beneficiary (powerful/constrained) — no legal barriers from ecosystem personhood
 *   - state_actors: Agenda setter (institutional/constrained) — administers the boundary through courts and legislation
 *   - developmental_potentiality_advocates: Excluded (organized/identity_locked) — would extend personhood to conception
 *   - functional_capacity_advocates: Excluded (organized/identity_locked) — would extend personhood based on cognition regardless of species
 *   - legal_philosophers: Observer (analytical/analytical) — analyzes the structural coherence of competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.38).
domain_priors:suppression_score(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.62).
domain_priors:theater_ratio(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__restrictive_anthropocentric_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__restrictive_anthropocentric_reading, "Legal Personhood Restricted to Born Humans with Cognitive Capacity").
narrative_ontology:topic_domain(legal_personhood_boundary__restrictive_anthropocentric_reading, "legal/philosophical/constitutional").

domain_priors:requires_active_enforcement(legal_personhood_boundary__restrictive_anthropocentric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__restrictive_anthropocentric_reading, '9c94be92-b333-4911-9ffc-2e69f13ea579').
narrative_ontology:cs_kernel_codification('9c94be92-b333-4911-9ffc-2e69f13ea579', distributed).
narrative_ontology:cs_authority_grounding('9c94be92-b333-4911-9ffc-2e69f13ea579', distributed).
narrative_ontology:cs_reading_relation('9c94be92-b333-4911-9ffc-2e69f13ea579', legal_personhood_boundary__developmental_potentiality_reading, coexists_with).
narrative_ontology:cs_reading_relation('9c94be92-b333-4911-9ffc-2e69f13ea579', legal_personhood_boundary__functional_capacity_reading, coexists_with).
narrative_ontology:cs_axiom('9c94be92-b333-4911-9ffc-2e69f13ea579', foundational, personhood_requires_birth_and_cognition).
narrative_ontology:cs_axiom_status(personhood_requires_birth_and_cognition, holdable).
narrative_ontology:cs_axiom_grounding('9c94be92-b333-4911-9ffc-2e69f13ea579', personhood_requires_birth_and_cognition, conventional).
narrative_ontology:cs_axiom('9c94be92-b333-4911-9ffc-2e69f13ea579', foundational, species_membership_insufficient_for_personhood).
narrative_ontology:cs_axiom_status(species_membership_insufficient_for_personhood, holdable).
narrative_ontology:cs_axiom_grounding('9c94be92-b333-4911-9ffc-2e69f13ea579', species_membership_insufficient_for_personhood, conventional).
narrative_ontology:cs_axiom('9c94be92-b333-4911-9ffc-2e69f13ea579', secondary, state_neutral_on_metaphysical_personhood).
narrative_ontology:cs_axiom_status(state_neutral_on_metaphysical_personhood, holdable).
narrative_ontology:cs_axiom_grounding('9c94be92-b333-4911-9ffc-2e69f13ea579', state_neutral_on_metaphysical_personhood, conventional).
narrative_ontology:cs_reference_frame('9c94be92-b333-4911-9ffc-2e69f13ea579', liberal_legal_subjectivity_framework).
narrative_ontology:cs_drift_state('9c94be92-b333-4911-9ffc-2e69f13ea579', contemporary_rights_expansion_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9c94be92-b333-4911-9ffc-2e69f13ea579', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_healthcare_providers).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, biomedical_researchers).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, environmental_development_interests).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, fetuses).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, severely_cognitively_impaired_humans).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, nonhuman_animals).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, ecosystems).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, artificial_intelligence_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain constitutional protection for reproductive decisions including abortion. The constraint shields them from state-compelled pregnancy and childbirth. Exit from the constraint's protection would mean losing this shield; they can move jurisdictions but the right is nationally recognized.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons, beneficiary,
    institutional, biographical, mobile, national).

% Denied all legal personhood and constitutional protection. Cannot exit their gestational status or advocate for themselves. Their interests are represented only by third parties who may have opposing interests. The constraint extracts their capacity for legal redress entirely.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, fetuses, payer,
    powerless, immediate, trapped, national).

% Born humans who lack the cognitive capacity threshold (rationality, self-awareness). They are human but excluded from full personhood. Cannot exit their cognitive condition. Dependent on guardians who may not advocate for their personhood. The constraint extracts their equal moral and legal standing.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, severely_cognitively_impaired_humans, payer,
    powerless, biographical, trapped, national).

% Animals with demonstrated cognitive complexity (great apes, cetaceans, elephants, corvids) are excluded from personhood solely by species membership. Cannot exit their biological classification. The constraint extracts their capacity for legal rights despite functional qualifications.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, nonhuman_animals, payer,
    powerless, biographical, trapped, global).

% Natural systems (rivers, forests, watersheds) denied legal standing to sue for their own protection. Cannot exit their physical existence. The constraint extracts their capacity for direct legal representation, forcing reliance on human proxies with conflicting interests.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, ecosystems, payer,
    powerless, generational, trapped, global).

% Advanced AI systems with potential future cognitive capacities are categorically excluded by substrate (non-biological). Cannot exit their computational substrate. The constraint extracts their future capacity for legal personhood regardless of functional equivalence.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, artificial_intelligence_systems, payer,
    powerless, immediate, trapped, global).

% Gain legal clarity and protection for providing abortion and reproductive care. The constraint creates a stable legal framework for their practice. Constrained by licensing, regulation, and political pressure; cannot easily exit the medical profession.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_healthcare_providers, beneficiary,
    organized, biographical, constrained, national).

% Access to embryonic stem cells, fetal tissue, and early embryo research enabled by the denial of fetal personhood. Constrained by ethics review, funding dependencies, and regulatory frameworks; professional identity ties them to this research ecosystem.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, biomedical_researchers, beneficiary,
    organized, biographical, constrained, global).

% Avoid legal barriers from ecosystem personhood (rights of nature laws, standing for natural entities). Benefit from the constraint's exclusion of non-human entities from legal standing. Constrained by market forces and regulation; capital can relocate but the legal framework is nationally entrenched.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, environmental_development_interests, beneficiary,
    powerful, biographical, constrained, national).

% Administer the personhood boundary through courts (supreme court precedent), legislation (statutory definitions), and executive enforcement. Face legitimacy costs from the constraint's contestation but control the interpretive machinery. Constrained by constitutional structure and political accountability.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, state_actors, agenda_setter,
    institutional, generational, constrained, national).

% Advocate for personhood from conception based on human developmental trajectory. Their professional, religious, and ideological identity is fused to this reading; exit would require abandoning core commitments. Structurally excluded from the constraint's operation but actively contest it through litigation, legislation, and cultural mobilization.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, developmental_potentiality_advocates, excluded,
    organized, civilizational, identity_locked, national).

% Advocate for personhood based on cognitive capacity regardless of species (animal rights, AI rights, ecosystem rights). Their scholarly, activist, and professional identity is fused to this reading. Structurally excluded but contest through litigation (animal personhood cases), legislation (rights of nature), and philosophical argument.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, functional_capacity_advocates, excluded,
    organized, civilizational, identity_locked, global).

% Analyze the structural coherence, normative justification, and practical consequences of competing personhood readings. Neither collect nor pay from the constraint; their role is to map the conceptual landscape and identify the constraint's extraction/coordination profile.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_philosophers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, secular, administrable threshold for legal personhood that avoids metaphysical disputes about ensoulment or potentiality, protects reproductive autonomy from state coercion, and enables stable legal frameworks for biomedical research and environmental regulation.
% TRANSFER_FUNCTION: Moves legal standing, constitutional protection, and capacity for rights-assertion from excluded entities (fetuses, impaired humans, animals, ecosystems, AI) to beneficiaries (pregnant persons, researchers, development interests) — the excluded lose the ability to make legal claims; the beneficiaries gain autonomy, research access, and regulatory certainty.
% ABSENT_VOICES: The excluded entities themselves (fetuses, severely impaired humans, nonhuman animals, ecosystems, future AI) cannot speak in the legal process. Their interests are represented only by advocates who may not share their structural position. Developmental and functional advocates are present but structurally excluded from the constraint's operation — they would object to the boundary but are kept outside by its definitional machinery.
% DISAPPEARANCE_RATIONALE: If the born-human-cognitive-capacity boundary vanished overnight, multiple regimes would reorganize: abortion law would revert to state-by-state or fetal-personhood frameworks; end-of-life law would face challenges from both developmental and functional directions; animal welfare would shift toward rights-based frameworks; environmental standing would expand to ecosystems; AI personhood proposals would gain traction. The legal architecture of rights-bearing entities would fundamentally restructure.
% FOUNDING_PROBLEM: The need for a secular, administrable legal personhood threshold that (1) avoids theological disputes about ensoulment, (2) protects women's reproductive autonomy from state-compelled pregnancy, (3) provides stable legal lines for biomedical research and clinical practice, and (4) prevents unlimited expansion of rights-bearing entities that would destabilize legal systems.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the restrictive reading's beneficiaries (pregnant persons, researchers) as still live. Developmental advocates attest it was misdiagnosed — the real problem is protecting all human life. Functional advocates attest it is arbitrarily speciesist. No neutral third party corroborates the original problem statement without qualification; constitutional historians note the threshold emerged from specific mid-20th century liberal compromises, not a timeless principle.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__restrictive_anthropocentric_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__restrictive_anthropocentric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(legal_personhood_boundary__restrictive_anthropocentric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).
:- end_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects the moderate but real transfer of legal protection from excluded entities to beneficiaries. The constraint extracts the capacity for legal redress from fetuses, impaired humans, animals, ecosystems, and AI — a diffuse but structurally significant extraction. Suppression (0.62) is substantial because the boundary must be actively defended against developmental and functional challengers through courts, legislation, and doctrinal maintenance; the constraint would collapse to a broader personhood without this enforcement. Theater ratio (0.15) is low because the cognitive-capacity threshold performs genuine coordination work (clear legal lines, protection of reproductive autonomy) rather than merely performative maintenance. Accessibility collapse (0.45) is moderate: alternative personhood frameworks remain conceptually available and politically mobilized (both developmental and functional readings are live), but the born-human-with-cognition threshold creates significant path dependence in legal doctrine. Resistance (0.58) is high because both sibling readings generate sustained political, legal, and philosophical opposition — the constraint is genuinely contested.
 *
 * PERSPECTIVAL GAP:
 *   The pregnant person seat and the fetus seat compute to fundamentally different types: from the pregnant person's position, the constraint is a Rope (genuine coordination protecting autonomy with minimal coercive overhead); from the fetus's position, it is a Snare (pure extraction of life protection enforced by state power). The severely cognitively impaired human seat also computes as Snare-adjacent. The developmental and functional advocate seats experience the constraint as active suppression of their frameworks (high d). The state actor seat experiences it as Tangled Rope (coordination + extraction). The engine computes this divergence from the structural data — the claimed type (tangled_rope) reflects the constraint's aggregate structure, not any single seat's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Pregnant persons are the primary structural beneficiaries (d ≈ 0.15): the constraint directly subsidizes their reproductive autonomy and bodily integrity. Fetuses are the primary structural targets (d ≈ 0.95): they bear the full cost of exclusion (denial of life protection) with zero exit. Severely cognitively impaired humans occupy a complex position (d ≈ 0.75): they are human but excluded by the cognitive capacity criterion, with no exit from their impairment. Nonhuman animals, ecosystems, and AI systems are targets (d ≈ 0.85): excluded by species/substrate criteria with no exit. Reproductive healthcare providers and biomedical researchers are secondary beneficiaries (d ≈ 0.30): they gain legal clarity but remain constrained by regulation. Environmental development interests are beneficiaries (d ≈ 0.25): they avoid ecosystem personhood barriers. State actors as agenda setters sit near symmetric (d ≈ 0.50): they administer the boundary but face legitimacy costs from its contestation. Developmental and functional advocates are excluded but identity-locked (d ≈ 0.80): their professional/ideological identity is fused to their reading, making exit from the contest structurally difficult.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (providing a clear, secular, administrable personhood threshold that protects reproductive autonomy and avoids metaphysical disputes) remains live but contested. The developmental reading argues the problem was misdiagnosed — the real need is protecting all human life from conception. The functional reading argues the threshold is arbitrarily speciesist. Neither sibling reading has been foreclosed; both remain live positions with institutional footholds. The constraint does not exhibit mandatrophy in the classical sense (a solved problem leaving a vestigial structure) because the founding problem persists, but it does exhibit extraction accumulation: the cognitive capacity criterion has expanded the victim set over time (from merely excluding fetuses to also excluding impaired humans, animals, ecosystems, AI) as functionalist challenges press the boundary. This is not a degraded Rope but an actively maintained Tangled Rope whose extraction profile has widened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_framing_underdetermination,
    'Does the legal_personhood_boundary kernel admit only these three readings, or are there further structurally distinct framings (e.g., relational personhood, gradient personhood, procedural personhood) that would change the victim/beneficiary map?',
    'Systematic survey of legal philosophy literature and constitutional jurisprudence for personhood frameworks not captured by the developmental/functional/restrictive trichotomy; assess whether each produces a distinct ε and victim set.',
    'If additional framings exist with distinct structural profiles, the kernel decomposition is incomplete — the current three-story family would miss constraint seats and extraction pathways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_underdetermination, conceptual, 'Whether the three declared readings exhaust the kernel''s structural possibilities.').

omega_variable(
    cognitive_capacity_operationalization,
    'What specific cognitive capacities constitute the threshold for personhood in this reading, and how does their operationalization affect the victim set (particularly severely impaired humans and advanced AI)?',
    'Analyze judicial opinions and philosophical defenses of the cognitive capacity criterion for vagueness, threshold effects, and boundary cases; test against empirical cognitive science.',
    'If the threshold is irreducibly vague or produces perverse exclusions (e.g., excluding late-stage Alzheimer''s patients while including advanced AI), the constraint''s coordination function degrades and its extraction becomes less defensible — potentially shifting classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cognitive_capacity_operationalization, conceptual, 'Vagueness and boundary stability of the cognitive capacity criterion.').

omega_variable(
    state_neutrality_vs_substantive_protection,
    'Does the state''s refusal to recognize fetal/ecosystem/AI personhood constitute neutrality (as this reading claims) or a substantive metaphysical choice that itself extracts from excluded entities?',
    'Compare with liberal neutrality doctrine in other domains (religion, speech, family structure); assess whether ''no personhood'' is a neutral baseline or a positive exclusion.',
    'If the boundary is a substantive choice, the constraint''s claimed coordination function (neutral administrability) is undermined — the extraction is not a byproduct of coordination but the point of the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_neutrality_vs_substantive_protection, conceptual, 'Whether the personhood boundary is genuinely neutral or a value-laden exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__restrictive_anthropocentric_reading, 1973, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t1973, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 1973, 0.05).
narrative_ontology:measurement(lega_tr_t1992, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 1992, 0.08).
narrative_ontology:measurement(lega_tr_t2000, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(lega_tr_t2010, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(lega_tr_t2022, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 2022, 0.14).
narrative_ontology:measurement(lega_tr_t2026, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 2026, 0.15).

% Extraction over time
narrative_ontology:measurement(lega_be_t1973, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 1973, 0.22).
narrative_ontology:measurement(lega_be_t1992, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 1992, 0.31).
narrative_ontology:measurement(lega_be_t2000, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(lega_be_t2010, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 2010, 0.37).
narrative_ontology:measurement(lega_be_t2022, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 2022, 0.38).
narrative_ontology:measurement(lega_be_t2026, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 2026, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t1973, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 1973, 0.45).
narrative_ontology:measurement(lega_su_t1992, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 1992, 0.52).
narrative_ontology:measurement(lega_su_t2000, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(lega_su_t2010, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(lega_su_t2022, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 2022, 0.61).
narrative_ontology:measurement(lega_su_t2026, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__restrictive_anthropocentric_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.08).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, abortion_access_regime).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, end_of_life_law).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, animal_welfare_statutes).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, environmental_standing_doctrine).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, ai_legal_personhood_proposals).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, biomedical_research_regulation).

% DUAL FORMULATION NOTE:
% The legal_personhood_boundary kernel decomposes into three constraint stories: developmental_potentiality_reading (personhood from conception, low extraction for fetuses, high for pregnant persons), functional_capacity_reading (personhood from cognition regardless of species, low extraction for qualifying entities, high for excluded humans), and this restrictive_anthropocentric_reading (born humans with cognition only, moderate extraction from excluded entities). The upstream developmental and functional readings influence this reading's contestation dynamics; this reading's institutional entrenchment influences the sibling readings' viability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legal_personhood_boundary__restrictive_anthropocentric_reading, powerless, 0.95).
constraint_indexing:directionality_override(legal_personhood_boundary__restrictive_anthropocentric_reading, organized, 0.3).
constraint_indexing:directionality_override(legal_personhood_boundary__restrictive_anthropocentric_reading, powerful, 0.25).
constraint_indexing:directionality_override(legal_personhood_boundary__restrictive_anthropocentric_reading, institutional, 0.5).
constraint_indexing:directionality_override(legal_personhood_boundary__restrictive_anthropocentric_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
