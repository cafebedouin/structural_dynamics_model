% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__modernist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__modernist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: orthographic_legitimacy_kernel__modernist_reading
 *   human_readable: Orthographic Legitimacy from Western Modernity Rupture (Modernist Reading)
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   This constraint story captures the modernist reading of the orthographic
 *   legitimacy kernel: the claim that a nation's script derives legitimacy
 *   from its alignment with Western/European modernity and its explicit
 *   rupture from the Ottoman/Islamic past. The paradigmatic case is the
 *   Turkish alphabet reform (1928), but the reading structures script reforms
 *   across the post-Ottoman and post-colonial Muslim world. The modernizing
 *   state apparatus imposes Latinization (or Cyrillization) as a constitutive
 *   act of national identity formation — not merely an administrative
 *   efficiency. The coordination function is real: a shared script enables
 *   mass literacy, print capitalism, and national communication. The
 *   extraction is asymmetric: the Ottoman literate class (bureaucrats,
 *   religious scholars, calligraphers) is rendered professionally and
 *   culturally illiterate; their human capital is confiscated without
 *   compensation. The constraint requires active enforcement (banning Arabic
 *   script, mandating Latin script in education/law/administration) and meets
 *   sustained resistance from traditional elites and religious communities.
 *   The claimed_type is tangled_rope: genuine coordination fused with
 *   asymmetric extraction.
 *
 * KEY AGENTS:
 *   - modernizing_state_apparatus: Primary agenda_setter and beneficiary (institutional/arbitrage) — designs and enforces the reform, captures legitimacy and state capacity gains
 *   - ottoman_literate_class: Primary payer (powerless/trapped) — bureaucrats, scribes, administrators whose literacy capital is expropriated
 *   - religious_scholars: Primary payer (moderate/identity_locked) — ulema, medrese teachers whose authority rests on Arabic-script textual tradition
 *   - general_population: Dual beneficiary/payer (organized/constrained) — gains mass literacy access but bears transition costs and cultural dislocation
 *   - minority_communities: Excluded (moderate/trapped) — Armenian, Greek, Jewish communities with distinct script traditions marginalized by unitary national script
 *   - international_observers: Observer (analytical/analytical) — foreign diplomats, linguists, later scholars analyzing the reform
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__modernist_reading, 0.78).
domain_priors:suppression_score(orthographic_legitimacy_kernel__modernist_reading, 0.85).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__modernist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__modernist_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__modernist_reading, "Orthographic Legitimacy from Western Modernity Rupture (Modernist Reading)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__modernist_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__modernist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__modernist_reading, '63a3a718-8364-4628-a35b-3b9763f6d66b').
narrative_ontology:cs_kernel_codification('63a3a718-8364-4628-a35b-3b9763f6d66b', formalized).
narrative_ontology:cs_authority_grounding('63a3a718-8364-4628-a35b-3b9763f6d66b', extraction).
narrative_ontology:cs_interpretation_layer_present('63a3a718-8364-4628-a35b-3b9763f6d66b').
narrative_ontology:cs_reading_relation('63a3a718-8364-4628-a35b-3b9763f6d66b', orthographic_legitimacy_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('63a3a718-8364-4628-a35b-3b9763f6d66b', orthographic_legitimacy_kernel__instrumentalist_reading, coexists_with).
narrative_ontology:cs_axiom('63a3a718-8364-4628-a35b-3b9763f6d66b', foundational, legitimacy_derives_from_modernity_rupture).
narrative_ontology:cs_axiom_status(legitimacy_derives_from_modernity_rupture, holdable).
narrative_ontology:cs_axiom_grounding('63a3a718-8364-4628-a35b-3b9763f6d66b', legitimacy_derives_from_modernity_rupture, conventional).
narrative_ontology:cs_axiom('63a3a718-8364-4628-a35b-3b9763f6d66b', secondary, ottoman_past_is_illegitimate_heritage).
narrative_ontology:cs_axiom_status(ottoman_past_is_illegitimate_heritage, holdable).
narrative_ontology:cs_axiom_grounding('63a3a718-8364-4628-a35b-3b9763f6d66b', ottoman_past_is_illegitimate_heritage, conventional).
narrative_ontology:cs_reference_frame('63a3a718-8364-4628-a35b-3b9763f6d66b', western_modernity_alignment).
narrative_ontology:cs_drift_state('63a3a718-8364-4628-a35b-3b9763f6d66b', contemporary_neo_ottoman_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('63a3a718-8364-4628-a35b-3b9763f6d66b', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, general_population).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, ottoman_literate_class).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, religious_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, general_population).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__modernist_reading, national_identity_requires_script_rupture).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__modernist_reading, modernization_requires_western_alignment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces the script reform through legislative acts, educational monopoly, and administrative mandate. Captures legitimacy as the 'modernizer' and gains state capacity through unified national literacy. Can pivot policy if legitimacy erodes (arbitrage exit), but has invested generational credibility in the reform.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus, beneficiary).

% Bureaucrats, scribes, court officials, and administrators whose professional literacy is in Arabic script. The reform renders their human capital obsolete overnight. No retraining pathway provided; too old for new education; no alternative employment using their skills. Exit is blocked by age, specialization, and the totalizing scope of the reform.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, ottoman_literate_class, payer,
    powerless, biographical, trapped, national).

% Ulema, medrese teachers, and Islamic scholars whose authority derives from mastery of Arabic-script textual tradition (Quran, hadith, fiqh). The reform severs their connection to the textual sources of their authority. Exit would require abandoning their vocational identity — the scholar role IS Arabic-script literacy. Some adapt by learning Latin script, but the structural position is identity-locked.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, religious_scholars, payer,
    moderate, generational, identity_locked, national).

% Gains access to mass literacy, print media, and state services through Latin script — a genuine coordination benefit. But bears transition costs: parents cannot help children with homework, family archives become unreadable, cultural continuity with grandparents is severed. Exit is constrained: they cannot opt out of state education or legal documentation, but can maintain private Arabic-script practices.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, general_population, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__modernist_reading, general_population, payer).

% Armenian, Greek, and Jewish communities with distinct script traditions (Armenian, Greek, Hebrew alphabets). The unitary national script policy marginalizes their educational and cultural institutions. They are not targets of the reform's extraction (their scripts are not the primary object) but are structurally excluded from the coordination benefit. Exit is trapped: emigration is the only full exit.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, minority_communities, excluded,
    moderate, generational, trapped, national).

% Foreign diplomats, linguists, and later scholars who analyze the reform as a case study in script change, nation-building, and cultural engineering. They neither collect nor pay; their seat provides the analytical perspective that distinguishes the modernist reading from its siblings.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, international_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__modernist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a unified national literacy space enabling mass education, print capitalism, legal codification, and bureaucratic communication — solving the fragmentation of Ottoman multilingual/multiscript administration by imposing a single Latin-based script aligned with Western modernity.
% TRANSFER_FUNCTION: Confiscates the cultural-professional capital of the Ottoman literate class (bureaucrats, scholars, scribes) and transfers legitimacy/rents to the modernizing state apparatus. The general population pays transition costs (relearning, cultural dislocation) and receives literacy access. Minority communities pay exclusion costs without compensatory inclusion.
% ABSENT_VOICES: The Ottoman literate class (eliminated as a class within a generation) and religious scholars (structurally silenced by educational monopoly) are the primary absent voices. Minority communities' script traditions were excluded from the national framework. Their objection would be: the coordination function could have been achieved through gradual bilingual transition without the constitutive rupture that defines the modernist reading's legitimacy claim.
% DISAPPEARANCE_RATIONALE: If the Latin script mandate and Arabic script prohibition vanished overnight, the legal/educational/administrative infrastructure would collapse into script pluralism. The state would lose its primary symbolic rupture from the Ottoman past. Religious education would revert to Arabic script. Minority script schools would demand equal recognition. The national identity constituted through script unity would fracture.
% FOUNDING_PROBLEM: The Ottoman Empire's multilingual, multiscript administration hindered centralized state capacity, mass mobilization, and national identity formation. The Arabic script was seen as tethering the population to Islamic-Ottoman cultural hegemony and preventing Western-style modernization.
% FOUNDING_PROBLEM_CORROBORATION: Turkish historical scholarship (outside the Kemalist establishment) documents that mass literacy was achieved by the 1960s and the Ottoman literate class had disappeared by the 1950s. The founding problem (creating unified national literacy) is acknowledged as solved by independent historians. The Kemalist establishment continues to assert the problem is live (ongoing 'cultural threat' from Arabic script), but this is self-assertion without external corroboration.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__modernist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__modernist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__modernist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__modernist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__modernist_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__modernist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_legitimacy_kernel__modernist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_legitimacy_kernel__modernist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the reform confiscates the cultural capital of the Ottoman literate class without compensation and imposes transition costs on the general population. Suppression is very high (0.85) because the reform's persistence depends on active legal prohibition of the old script, educational monopoly, and criminalization of Arabic-script usage — not voluntary adoption. Theater ratio is low (0.22): the coordination function (national identity, mass literacy) is genuinely performed, not merely performative. Accessibility collapse (0.68) is substantial but not total: Ottoman script persists in religious practice, calligraphy, and scholarly work, but is excluded from public/legal/educational spheres. Resistance (0.72) is high: traditional elites, religious institutions, and minority communities resisted through petitions, parallel education, and cultural preservation. The measurement series shows extraction rising as the reform consolidates (0.45→0.78), suppression peaking early then stabilizing (0.9→0.85), theater creeping up slightly as ceremonial 'script festivals' replace functional coordination.
 *
 * PERSPECTIVAL GAP:
 *   The modernizing state apparatus (agenda_setter/beneficiary) experiences the constraint as rope: it built the coordination mechanism, controls its enforcement, and collects the legitimacy gains. The Ottoman literate class and religious scholars (payers) experience it as snare: their exit is blocked (identity_locked for scholars, trapped for bureaucrats), the coordination story is cover for their displacement. The general population (dual role) sits near symmetric: genuine literacy gains offset by cultural rupture costs. The engine computes this divergence from the structural data — the modernist reading's axioms declare the rupture as legitimate, but the payer seats compute high effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The modernizing state apparatus is the structural beneficiary (d ≈ 0.1): it authors the constraint, controls enforcement, captures state capacity and legitimacy rents. The Ottoman literate class and religious scholars are full targets (d ≈ 0.9): they bear the extraction, their exit is structurally blocked (trapped/identity_locked), and the constraint's persistence depends on their continued subordination. The general population is near symmetric (d ≈ 0.5): they gain literacy access (coordination benefit) but pay transition costs and cultural dislocation. Minority communities are excluded from the coordination entirely (d ≈ 0.8): their script traditions are suppressed without compensatory inclusion. The derivation chain from beneficiary/victim declarations + exit options produces these directionalities; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (creating a unified national literacy and breaking Ottoman-Islamic cultural hegemony) was live in the 1920s-30s. By the 1960s, mass literacy was achieved and the Ottoman literate class had died out — the founding problem is dead. Yet the constraint persists with full enforcement (no sunset clause, no transition to voluntary script choice). The mandatrophy is unresolved: the coordination function (literacy) is achieved, but the extraction function (suppression of Arabic script) continues without founding justification. This is the tangled_rope signature: coordination achieved, extraction locked in.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is the modernist_reading of the orthographic_legitimacy_kernel. What structural elements distinguish it from the continuity_reading and instrumentalist_reading?',
    'Comparative analysis of the three readings'' beneficiary/victim structures, coordination functions, and founding axioms. The kernel context identifies the modernist reading as constitutive of national identity transformation via rupture.',
    'Confirms this reading instantiates a distinct constraint with high extraction from traditional elites and beneficiary structure centered on the modernizing state apparatus. Prevents conflation with efficiency-based or tradition-preserving framings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system framing: this constraint is one reading of a contested kernel, not the kernel itself.').

omega_variable(
    coordination_extraction_boundary,
    'Is the script reform''s coordination function (national identity formation via Western alignment) structurally separable from its extraction function (rendering Ottoman literate class illiterate), or are they constitutively fused?',
    'Counterfactual analysis: if the state had pursued Latinization without suppressing Ottoman script literacy (e.g., bilingual transition), would the national identity coordination still succeed? Historical comparison with gradual script transitions.',
    'If fused, the high extraction is the price of coordination (tangled_rope confirmed). If separable, the suppression of Ottoman literacy is gratuitous extraction riding on a genuine coordination function (snare dynamics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether coordination and extraction are constitutively linked in this reading''s structure.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal bans, educational replacement, state enforcement) or partially internalized (traditional elites accepting their obsolescence, population internalizing Western script as ''modern'')?',
    'Post-reform trajectory analysis: if suppression persists after state enforcement relaxes (e.g., Ottoman script remains marginalized without active bans), internalized component is significant.',
    'If substantially internalized, effective suppression exceeds structural measure — the constraint carries its own reproduction mechanism beyond state capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in script reform.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__modernist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(orth_tr_t5, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(orth_tr_t10, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(orth_tr_t15, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(orth_tr_t20, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(orth_be_t5, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(orth_be_t10, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement(orth_be_t15, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 15, 0.76).
narrative_ontology:measurement(orth_be_t20, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 20, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(orth_su_t5, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 5, 0.88).
narrative_ontology:measurement(orth_su_t10, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(orth_su_t15, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 15, 0.85).
narrative_ontology:measurement(orth_su_t20, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 20, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__modernist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_legitimacy_kernel__modernist_reading, 0.08).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel__instrumentalist_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial label 'orthographic legitimacy' into three structurally distinct claims with different ε values, beneficiary/victim structures, and coordination functions. The modernist reading (this story) has high extraction from traditional elites; the continuity reading has near-zero extraction but high suppression of reform; the instrumentalist reading has low extraction but contested coordination efficacy. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
