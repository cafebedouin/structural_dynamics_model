% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__durable_separation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_durable_separation, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: herem_command_dt7__durable_separation_reading
 *   human_readable: Herem Divine Command: Durable Separation Mandate
 *   domain: religious_ethics/commitment_system
 *
 * SUMMARY:
 *   The durable-separation reading of Herem interprets the biblical command
 *   to maintain categorical separation from non-covenant outsiders
 *   (Deuteronomy 7) as a timeless divine mandate for identity preservation.
 *   Under this reading, Herem encodes a binding, unchangeable law: the
 *   covenant community must remain ethnically and religiously pure through
 *   prohibition of intermarriage, exclusion of outsiders from sacred goods,
 *   and justified violence against those who violate boundaries. The reading
 *   claims this mandate is divinely authored, categorically binding, and not
 *   historically contingent. This JSON instantiates this single reading as a
 *   constraint story independent of its sibling readings
 *   (contextual-supersession and allegorical-displacement), which are
 *   separate constraint files. The ε value (0.82) is measured against the
 *   standing arrangement THIS reading describes—the categorical separation
 *   regime as separatist leadership understands it—not against a universalist
 *   alternative the reading would reject. Extractiveness is high because the
 *   reading extracts substantial relational autonomy from intermarriage
 *   adherents and expands the victim set to include all non-covenant
 *   outsiders as permanent contamination threats.
 *
 * KEY AGENTS:
 *   - Separatist leadership: institutional actors (rabbinical authorities, clerical hierarchies, boundary-guard interpreters) who enforce and adjudicate Herem's mandates. Power: institutional. Exit: mobile (could adopt alternative readings but that would mean abandoning the interpretive tradition). Benefit from moral authority and institutional control grounded in boundary-guardianship.
 *   - Intermarriage adherents: moderate-power community members whose relational autonomy is directly extracted. Identity-locked (exit means community dissolution). Face suppression of marriage/partnership choice.
 *   - Non-covenant outsiders: powerless category, structurally excluded, treated as contamination threat. Permanent victim set under this reading.
 *   - Boundary-transgression sympathizers: trapped between theological acceptance of Herem's authority and moral rejection of its extraction. Suppressed voices (cannot voice doubt without risking membership). Secondary role: excluded.
 *   - Universalist prophetic traditions: powerful alternative readings (excluded from this reading's framework) that would supersede or allegorize Herem. Their exclusion is structural—the reading defines itself against them.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, 0.82).
domain_priors:suppression_score(herem_command_dt7__durable_separation_reading, 0.88).
domain_priors:theater_ratio(herem_command_dt7__durable_separation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__durable_separation_reading, tangled_rope).
narrative_ontology:human_readable(herem_command_dt7__durable_separation_reading, "Herem Divine Command: Durable Separation Mandate").
narrative_ontology:topic_domain(herem_command_dt7__durable_separation_reading, "religious_ethics/commitment_system").

domain_priors:requires_active_enforcement(herem_command_dt7__durable_separation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__durable_separation_reading, 'e0e1d8cf-910b-4d64-86e5-82b380073c82').
narrative_ontology:cs_kernel_codification('e0e1d8cf-910b-4d64-86e5-82b380073c82', fixed_text).
narrative_ontology:cs_authority_grounding('e0e1d8cf-910b-4d64-86e5-82b380073c82', lineage).
narrative_ontology:cs_interpretation_layer_present('e0e1d8cf-910b-4d64-86e5-82b380073c82').
narrative_ontology:cs_reading_relation('e0e1d8cf-910b-4d64-86e5-82b380073c82', herem_command_dt7__contextual_supersession_reading, forecloses).
narrative_ontology:cs_reading_relation('e0e1d8cf-910b-4d64-86e5-82b380073c82', herem_command_dt7__allegorical_displacement_reading, forecloses).
narrative_ontology:cs_axiom('e0e1d8cf-910b-4d64-86e5-82b380073c82', foundational, divine_separation_mandate_timeless).
narrative_ontology:cs_axiom_status(divine_separation_mandate_timeless, holdable).
narrative_ontology:cs_axiom_grounding('e0e1d8cf-910b-4d64-86e5-82b380073c82', divine_separation_mandate_timeless, deontological).
narrative_ontology:cs_axiom('e0e1d8cf-910b-4d64-86e5-82b380073c82', foundational, outsider_categorical_contamination_threat).
narrative_ontology:cs_axiom_status(outsider_categorical_contamination_threat, holdable).
narrative_ontology:cs_axiom_grounding('e0e1d8cf-910b-4d64-86e5-82b380073c82', outsider_categorical_contamination_threat, theological).
narrative_ontology:cs_reference_frame('e0e1d8cf-910b-4d64-86e5-82b380073c82', herem_separation_mandate_divinely_binding).
narrative_ontology:cs_drift_state('e0e1d8cf-910b-4d64-86e5-82b380073c82', contemporary_pluralist_diaspora, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e0e1d8cf-910b-4d64-86e5-82b380073c82', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__durable_separation_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_community_identity).
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, separatist_leadership).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, non_covenant_outsiders).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, intermarriage_adherents).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, boundary_transgression_sympathizers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The abstract collective identity that Herem's reading defends: a bounded, categorically pure membership whose continuity depends on strict boundary maintenance and exclusion of intermarriage and outsider assimilation. This is not a concrete actor but the institutional good the reading's framework privileges.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_community_identity, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(herem_command_dt7__durable_separation_reading, covenant_community_identity).

% Religious scholars, clerical hierarchies, and community authorities who interpret and enforce Herem's boundary mandates. They adjudicate who belongs, police intermarriage, and justify exclusion through appeal to divine command. They benefit from the moral authority and institutional control that boundary-guardianship confers. Their exit from the separatist reading would require abandoning the interpretive tradition itself.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, separatist_leadership, agenda_setter,
    institutional, generational, mobile, national).

% Designated outsiders — those outside the covenant community by ethnicity, religion, or genealogy — who are categorized as sources of contamination and spiritual danger. They bear the cost of exclusion, restricted access to community goods, and justified treatment as threats to collective purity. Their exit option is assimilation or permanent separation; neither is costless.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, non_covenant_outsiders, payer,
    powerless, generational, constrained, universal).

% Community members who marry or form bonds with non-covenant partners. They face suppression of their relational autonomy, social ostracism, loss of community standing, and pressure to dissolve unions. Their exit from the constraint requires rejecting the community identity itself, which is fused with their own self-understanding.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, intermarriage_adherents, payer,
    moderate, biographical, identity_locked, national).

% Community members who intellectually accept the binding authority of Herem's separation mandate but experience it as costly, unjust, or obsolete. They are trapped between the pulling force of the reading's theological authority (which they partly accept) and their own moral intuitions about outsider humanity and relational autonomy. They are suppressed voices within the community because voicing doubt risks membership status.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, boundary_transgression_sympathizers, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__durable_separation_reading, boundary_transgression_sympathizers, excluded).

% Alternative theological traditions (prophetic universalism, Christian covenant theology, Quranic universalism) that read the same scriptural sources as superseding or allegorizing Herem's separation mandate. They are systematically excluded from the durable-separation reading's framework, unable to be heard within it without dissolving the reading itself. Their exclusion is structural: the reading defines itself by rejecting their hermeneutic authority.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, universalist_prophetic_voices, excluded,
    powerful, civilizational, trapped, universal).

% Scholars, ethicists, and historians examining the constraint from outside the reading's committer frame. They observe the extraction of relational autonomy, the expansion of the victim set to include all non-covenant humans, and the legitimation of violence through divine-command obedience. They can measure the constraint's operation without endorsing it.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__durable_separation_reading, separatist_leadership).
narrative_ontology:fixing_cost_class(herem_command_dt7__durable_separation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains covenant-community identity through categorical separation: defines membership criteria, enforces boundary markers (intermarriage prohibition, dietary/ritual distinction), and allocates sacred goods (Torah study, ritual participation, community standing) to in-group only. The coordination problem solved is: how does a minority identity persist across diaspora and centuries when assimilation pressures are constant?
% TRANSFER_FUNCTION: Extracts relational autonomy from intermarriage adherents and boundary-transgression sympathizers; extracts full social standing from non-covenant outsiders (who are permanently excluded); transfers moral authority and institutional control to separatist leadership; transfers identity stability and in-group belonging to covenant community members who comply with boundaries.
% ABSENT_VOICES: Universalist prophetic readings (Jeremiah, Jonah, rabbinical universalism, Christian Pauline theology, Quranic umma doctrine) are structurally excluded — their hermeneutic authority is denied by the durable-separation reading. Non-covenant outsiders have no voice in the tradition's own adjudication; outsider-sympathizers within the community face suppression. These are the voices the reading's framework actively silences.
% DISAPPEARANCE_RATIONALE: If Herem's separation mandate were no longer binding divine command, intermarriage would normalize, boundary-patrol enforcement would cease, and covenant-identity would reorganize around alternative foundation (shared law, shared land, shared history) rather than categorical purity. The reading's entire legitimation structure would collapse; leadership authority grounded in boundary-guardianship would erode.
% FOUNDING_PROBLEM: Ancient Israel faced diaspora risk and assimilation pressure from neighboring polytheistic cultures. Herem's categorical separation mandate encoded a survival strategy: preserve monotheistic covenant identity against absorptive pressures through strict boundaries and exclusion of outsider religious practice.
% FOUNDING_PROBLEM_CORROBORATION: Historians and archaeologists attest the founding problem was real in the ancient Levantine context. Separatist leaders attest the problem remains live: diaspora dispersion and intermarriage continue to threaten covenant identity. Universalist theologians and secular ethicists attest that the founding problem was historically-bounded and is superseded: modern covenant identity (Jewish, Christian, Islamic) has proven durable without categorical ethnic/religious separation and even with high intermarriage rates. No corroboration from outside the separatist reading's own benefiting parties supports the claim that Herem's mandate is timeless.
narrative_ontology:disappearance_verdict(herem_command_dt7__durable_separation_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__durable_separation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__durable_separation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(herem_command_dt7__durable_separation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__durable_separation_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__durable_separation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(herem_command_dt7__durable_separation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the reading extracts relational autonomy from multiple constituencies (intermarriage adherents face suppression of spouse choice; outsiders are permanently excluded; sympathizers face pressure to conform). The extraction is justified through appeal to divine command (legitimate authority claim within the reading's framework) but is nonetheless experienced as coercive by those it targets. Suppression is even higher (0.88) because the constraint's persistence depends on active enforcement: boundary policing, social ostracism of violators, exclusion of alternative readings from hermeneutic authority. Theater ratio (0.41) is moderate-low: the constraint has real coordination content (identity continuity through boundary maintenance), but a growing share of measured enforcement activity goes to suppressing alternative readings and silencing internal doubt rather than defending against external assimilation. The temporal series shows extraction and suppression intensity rising across the interval, indicating both tightening enforcement and accumulating tension with universalist alternatives. This reading's extractiveness trajectory is opposite to what contextual-supersession would claim (that reading would show dropping extraction as the founding problem recedes); that divergence is exactly the kernel contest.
 *
 * PERSPECTIVAL GAP:
 *   From separatist leadership's seat, Herem is genuine coordination—a binding rule that preserves identity against assimilation. From intermarriage adherents' seat, the same structure operates as enforced extraction of relational autonomy justified by appeal to unquestionable authority. From non-covenant outsiders' seat (structurally excluded from the reading's framework), Herem is pure exclusion with no coordination benefit offered. From the analytical observer's seat, the reading's claimed coordination function (identity preservation) is real but comes at a cost (relational autonomy extraction) that the reading itself does not account for in its legitimation narrative. The engine computes these divergences from the structural data; the reading's own claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Separatist leadership sits near the beneficiary end (d ≈ 0.15): they collect institutional authority, define what binds the community, control sacred goods allocation. Intermarriage adherents sit near the target end (d ≈ 0.85): their relational autonomy is extracted, their exit is costly (identity-locked), their space for choice is constrained by the reading's authority claims. Non-covenant outsiders sit at the extreme target end (d ≈ 0.95): they are not offered membership on any terms; the constraint exists to keep them out. Boundary-transgression sympathizers sit high-target (d ≈ 0.78) because they are internally trapped: they partly accept the reading's authority but experience its extraction as unjust, yet exit means community dissolution. The covenant-community-identity as beneficiary (a non-agent entry) receives the stability benefit; individual community members who fully comply sit near symmetric (costs ≈ benefits) because they get identity continuity, but that symmetry is conditional on their compliance—the moment they question boundaries, they become targets. Universalist readings are structurally excluded (not offered a seat in the framework), which means they cannot be given a d value within this reading's logic; they are the reading's logical opponents.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is claimed as tangled_rope (possesses both coordination—identity stability through boundaries—and asymmetric extraction—autonomy suppression for intermarriage adherents). This classification prevents misreading the constraint as either pure rope (if the extraction were invisible) or pure snare (if the coordination function were invisibly atrophied). However, there is an omega question about whether the founding problem (assimilation pressure in diaspora) is still live or has been functionally solved by secular identity persistence and high intermarriage rates in modern diaspora communities. If the founding problem is dead while Herem's enforcement persists, the constraint would reclassify toward piton (theater ratio rising, enforcement increasingly performative). The measurement series does show theater ratio rising (0.25→0.41 across the interval), which is consistent with enforcement increasingly defending the reading's own authority rather than addressing external assimilation pressure. This creates mandatrophy risk: a constraint whose founding purpose has been superseded but whose enforcement persists and intensifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Is the founding problem (diaspora assimilation pressure threatening covenant identity) still live in modern contexts, or has it been functionally solved by secular identity frameworks and high intermarriage rates without covenant-identity dissolution?',
    'Empirical social science studies of diaspora community identity persistence, intermarriage rates, and religious continuity across generations. Comparison of assimilation outcomes in communities that enforce Herem strictness vs. those that have adopted universalist readings.',
    'If the founding problem is dead while Herem enforcement persists and intensifies, the constraint reclassifies from tangled_rope toward piton (coordination function atrophied, extraction now purely performative/inertial). If the problem is still live, the classification holds; if contested, the theater_ratio trajectory becomes the diagnostic signal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the founding problem Herem addresses is still functionally necessary or has been solved.').

omega_variable(
    reading_authority_grounding,
    'What grounds the durable-separation reading''s claim to divine timelessness? Is it the text''s plain grammatical sense, a continuous interpretive lineage, institutional authority consolidation, or theological axiom about divine law''s immutability?',
    'Hermeneutic analysis comparing textual evidence from Herem passages, historical-critical scholarship on the text''s composition and original context, and genealogy of the durable-separation reading''s appearance in rabbinic/theological tradition.',
    'If authority is grounded in interpretive lineage (not textual evidence), the reading becomes context-dependent, which would support the contextual-supersession reading''s framing. If authority is grounded in axiom (divine law is timeless by definition), the classification holds but the axiom becomes vulnerable to foundational challenge. If grounded in institutional consolidation (this reading won the authority competition), the constraint''s persistence depends on continued institutional dominance—piton risk.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_authority_grounding, conceptual, 'What epistemic and institutional grounds the reading''s authority claim.').

omega_variable(
    intermarriage_suppression_mechanism,
    'Is the suppression of intermarriage choice structural (legal barriers, social exclusion, economic disadvantage) or internalized (covenant members have fused their identity so completely with the reading that exit feels psychologically impossible)?',
    'Post-exit narrative analysis: do adherents who leave the separatist community report continued psychological suppression of intermarriage after formal exit from enforcement mechanisms? Or does suppression dissipate when legal/social barriers are removed?',
    'If suppression is primarily structural, removing legal enforcement might resolve it; if internalized, the constraint''s suppressive force persists beyond institutional enforcement—increasing theater_ratio as enforcement becomes more performative than functionally necessary. If mixed, the constraint''s reclassification would depend on the proportion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intermarriage_suppression_mechanism, empirical, 'Whether intermarriage suppression is structural or internalized identity-fusion.').

omega_variable(
    victim_set_scope_definition,
    'Does the durable-separation reading categorize as ''victims'' only those actively targeted for exclusion (non-covenant outsiders) and those who violate boundaries (intermarriage adherents), or does it extend the victim category to include all non-compliant sympathizers and potential boundary-transgressors?',
    'Textual and historical analysis of separatist leadership''s explicit categorizations: who is subject to enforcement machinery? Who is permitted to exist in non-compliant status without active suppression? Does the reading treat outsiders as ontologically permanent victims (never eligible for membership) or as conditionally external (eligible if they assimilate)?',
    'A narrower victim set (only active violators) would lower the constraint''s overall extractiveness; a universalized victim set (all non-covenant humanity as contamination threat) would support the high extractiveness claim (0.82). The scope definition also determines whether the constraint reclassifies as snare (if victim set is defined as all outsiders regardless of behavior) vs. tangled_rope (if victim set is bounded to those who violate specific rules).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_scope_definition, conceptual, 'The definitional scope of who counts as victim under this reading''s logic.').

omega_variable(
    kernel_contest_foreclosure_vs_coexistence,
    'Is this reading''s core claim (timeless divine separation mandate) logically incompatible with the sibling reading''s core claims (historical contingency, universalist supersession, allegorical displacement), or can the readings coexist as live alternative positions held by different parties?',
    'Theological analysis: can a single coherent framework (e.g., ''all these are valid readings from different eras'' or ''all are interpretations of an ambiguous text'') accommodate all three readings? Or does the durable-separation reading''s claim to timeless, categorical, divine authority logically foreclose any reading that treats Herem as contingent or non-literal?',
    'If the readings are logically incompatible (durable-separation forecloses the siblings), the kernel context shows genuine conflict at the level of foundational premises. If coexistent, the conflict is institutional/political rather than logical—different communities hold different readings, but no single framework must choose between them. The choice affects how to classify the kernel itself (is it a contested commitment or a genuinely fragmented one?) and whether merger/synthesis is theoretically possible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure_vs_coexistence, conceptual, 'Whether this reading''s core premise logically forecloses or coexists with its sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__durable_separation_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__durable_separation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(here_tr_t5, herem_command_dt7__durable_separation_reading, theater_ratio, 5, 0.29).
narrative_ontology:measurement(here_tr_t10, herem_command_dt7__durable_separation_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(here_tr_t15, herem_command_dt7__durable_separation_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(here_tr_t20, herem_command_dt7__durable_separation_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(here_tr_t25, herem_command_dt7__durable_separation_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__durable_separation_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(here_be_t5, herem_command_dt7__durable_separation_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement(here_be_t10, herem_command_dt7__durable_separation_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(here_be_t15, herem_command_dt7__durable_separation_reading, base_extractiveness, 15, 0.79).
narrative_ontology:measurement(here_be_t20, herem_command_dt7__durable_separation_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement(here_be_t25, herem_command_dt7__durable_separation_reading, base_extractiveness, 25, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__durable_separation_reading, suppression_requirement, 0, 0.79).
narrative_ontology:measurement(here_su_t5, herem_command_dt7__durable_separation_reading, suppression_requirement, 5, 0.81).
narrative_ontology:measurement(here_su_t10, herem_command_dt7__durable_separation_reading, suppression_requirement, 10, 0.83).
narrative_ontology:measurement(here_su_t15, herem_command_dt7__durable_separation_reading, suppression_requirement, 15, 0.85).
narrative_ontology:measurement(here_su_t20, herem_command_dt7__durable_separation_reading, suppression_requirement, 20, 0.87).
narrative_ontology:measurement(here_su_t25, herem_command_dt7__durable_separation_reading, suppression_requirement, 25, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__durable_separation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(herem_command_dt7__durable_separation_reading, 0.14).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, herem_command_dt7__contextual_supersession_reading).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, herem_command_dt7__allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% The herem_command_dt7 kernel admits three structurally distinct constraint readings. (1) DURABLE-SEPARATION (this file) claims high extractiveness (0.82) on intermarriage autonomy, timeless divine authority, and categorical outsider threat. (2) CONTEXTUAL-SUPERSESSION reads Herem as historically-bounded settlement directive, functionally superseded by prophetic universalism; this reading would show substantially lower extractiveness (~0.35-0.45) and classify as piton or scaffolded-then-dissolved. (3) ALLEGORICAL-DISPLACEMENT reads 'nations' as spiritual enemies not ethnic groups; conquest becomes internal moral struggle; extractiveness on relational autonomy would be near-zero (~0.05-0.15) and classification would be mountain or rope (genuine identity/virtue coordination). The three readings do NOT share a single ε-referent: each reading measures the SAME TEXT against its own epistemic framework, producing radically different structural assessments. The kernel contest is not observable-dependent; it is reading-dependent. All three must be authored as separate constraint files; this file instantiates only the durable-separation reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
