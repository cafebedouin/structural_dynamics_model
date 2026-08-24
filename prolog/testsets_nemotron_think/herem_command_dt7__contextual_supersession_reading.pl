% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__contextual_supersession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__contextual_supersession_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: herem_command_dt7__contextual_supersession_reading
 *   human_readable: Residual fundamentalist enforcement of herem-based ethnic separation and intermarriage prohibition
 *   domain: religious/ethical/historical
 *
 * SUMMARY:
 *   This constraint story models the residual enforcement of the biblical
 *   herem (Deuteronomy 7, 20) — the command to destroy Canaanite nations and
 *   prohibit intermarriage — as it operates in contemporary fundamentalist
 *   communities that treat the command as perpetually binding. The
 *   contextual_supersession_reading holds that herem was a historically
 *   bounded directive for Israel's settlement period, morally superseded by
 *   prophetic universalism (e.g., Amos 9:7, Jonah) and the Christian covenant
 *   (Acts 10, Galatians 3:28). From this reading's perspective, the
 *   continuing enforcement is an illegitimate survival: a snare that extracts
 *   compliance through shunning, excommunication, and social coercion while
 *   its legitimate coordination function (protecting a nascent covenant
 *   community in the Late Bronze Age) has been void for millennia. The
 *   constraint's extraction falls on fundamentalist members who desire
 *   exogamous marriage or dissent from the separation norm, and on outsiders
 *   stigmatized as 'Canaanite' by theological analogy. Fundamentalist leaders
 *   benefit from the boundary maintenance that consolidates authority. The
 *   claimed type is snare; the metrics reflect high extraction and
 *   suppression with moderate theater (the enforcement performs fidelity to a
 *   text the reading says no longer binds).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__contextual_supersession_reading, 0.72).
domain_priors:suppression_score(herem_command_dt7__contextual_supersession_reading, 0.78).
domain_priors:theater_ratio(herem_command_dt7__contextual_supersession_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, accessibility_collapse, 0.74).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__contextual_supersession_reading, snare).
narrative_ontology:human_readable(herem_command_dt7__contextual_supersession_reading, "Residual fundamentalist enforcement of herem-based ethnic separation and intermarriage prohibition").
narrative_ontology:topic_domain(herem_command_dt7__contextual_supersession_reading, "religious/ethical/historical").

domain_priors:requires_active_enforcement(herem_command_dt7__contextual_supersession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__contextual_supersession_reading, '37e10e75-9d71-4d4a-97a4-afc142dfe479').
narrative_ontology:cs_kernel_codification('37e10e75-9d71-4d4a-97a4-afc142dfe479', fixed_text).
narrative_ontology:cs_authority_grounding('37e10e75-9d71-4d4a-97a4-afc142dfe479', lineage).
narrative_ontology:cs_interpretation_layer_present('37e10e75-9d71-4d4a-97a4-afc142dfe479').
narrative_ontology:cs_reading_relation('37e10e75-9d71-4d4a-97a4-afc142dfe479', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('37e10e75-9d71-4d4a-97a4-afc142dfe479', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_axiom('37e10e75-9d71-4d4a-97a4-afc142dfe479', foundational, herem_command_historically_bounded_and_superseded).
narrative_ontology:cs_axiom_status(herem_command_historically_bounded_and_superseded, holdable).
narrative_ontology:cs_axiom_grounding('37e10e75-9d71-4d4a-97a4-afc142dfe479', herem_command_historically_bounded_and_superseded, theological).
narrative_ontology:cs_axiom('37e10e75-9d71-4d4a-97a4-afc142dfe479', secondary, prophetic_universalism_supersedes_ethnic_particularism).
narrative_ontology:cs_axiom_status(prophetic_universalism_supersedes_ethnic_particularism, holdable).
narrative_ontology:cs_axiom_grounding('37e10e75-9d71-4d4a-97a4-afc142dfe479', prophetic_universalism_supersedes_ethnic_particularism, theological).
narrative_ontology:cs_reference_frame('37e10e75-9d71-4d4a-97a4-afc142dfe479', prophetic_universalist_covenant).
narrative_ontology:cs_drift_state('37e10e75-9d71-4d4a-97a4-afc142dfe479', contemporary_fundamentalist_enforcement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('37e10e75-9d71-4d4a-97a4-afc142dfe479', '2026-08-15T12:00:00Z').
narrative_ontology:cs_kernel_id(herem_command_dt7__contextual_supersession_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, fundamentalist_leaders).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, fundamentalist_members).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, outsiders_excluded).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce the herem-based separation norm (prohibition on intermarriage with designated out-groups, shunning of dissenters). Justify enforcement as fidelity to biblical inerrancy and covenant faithfulness. Collect authority, tithes, and communal cohesion from the boundary maintenance. Could moderate the norm without personal cost but would lose the identity-coordination function that sustains their leadership.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, fundamentalist_leaders, agenda_setter,
    institutional, generational, arbitrage, global).

% Subject to the separation norm: cannot marry outside the community without severe sanctions (excommunication, shunning, loss of family, spiritual damnation narratives). Bear the cost of restricted marriage markets, suppressed dissent, and conformity labor. Exit means losing the self-concept and social world constituted by the community — identity_locked, not merely constrained. Some resist quietly; few leave.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, fundamentalist_members, payer,
    moderate, biographical, identity_locked, global).

% Stigmatized as 'Canaanite' or 'worldly' by theological analogy; excluded from community resources, marriage markets, and social recognition. Have no voice in the fundamentalist community's hermeneutics. Their exit options are trapped: they cannot join without converting (which requires accepting the separation norm), and they bear reputational costs from the designation.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, outsiders_excluded, payer,
    powerless, biographical, trapped, global).

% Hold the contextual_supersession_reading or allegorical_displacement_reading; do not enforce herem as a living norm. Experience neither extraction nor benefit from the fundamentalist enforcement. Provide the corroborating tradition (prophetic universalism, Christian covenant) that this reading cites as superseding the command.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, mainstream_believers, observer,
    organized, generational, analytical, global).

% Analyze the textual history, reception history, and ethical trajectory of the herem command. Provide the empirical and conceptual evidence for historical-boundedness and supersession. Do not bear costs or collect benefits from the fundamentalist enforcement.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, hermeneutical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its original historical context (Late Bronze Age / Iron Age transition), the herem command coordinated the survival of a nascent covenant community by enforcing ritual and marital boundaries against assimilation into surrounding polytheistic cultures. In the residual fundamentalist enforcement, the coordination function is atrophied: the boundary now primarily coordinates leader authority and group cohesion against modernity, not against Canaanite religion.
% TRANSFER_FUNCTION: Moves life choices (marriage, association, religious conscience) from fundamentalist members and outsiders to fundamentalist leaders, who convert the extracted conformity into authority, resource control, and communal durability. The transfer is enforced by shunning, excommunication, and eternal-consequence narratives.
% ABSENT_VOICES: The Canaanite nations designated in the original command (Hittites, Girgashites, Amorites, Canaanites, Perizzites, Hivites, Jebusites) are extinct as identifiable peoples — they cannot object. Contemporary Palestinians and other Levantine populations who might be analogized to 'Canaanites' by fundamentalist rhetoric are excluded from the conversation. Fundamentalist members who privately dissent but cannot speak are also absent voices (paired with their identity_locked exit).
% DISAPPEARANCE_RATIONALE: If the residual fundamentalist enforcement vanished overnight, fundamentalist communities would face immediate crisis of identity and authority: marriage markets would open, dissent would surface, leaders would lose the primary tool of boundary maintenance. Members would gain exit options; outsiders would lose the stigmatizing designation. The fundamentalist commitment system would either reorganize around a new boundary or fragment.
% FOUNDING_PROBLEM: Protecting the ritual purity and distinct identity of the nascent Israelite covenant community during the settlement period (c. 1200–1000 BCE) against assimilation into Canaanite polytheistic culture.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream Jewish and Christian traditions (Rabbinic Judaism: herem applies only to the Seven Nations who no longer exist; Christianity: superseded by the universal covenant in Christ) attest from outside the fundamentalist beneficiary set that the founding problem is historically superseded. Historical-critical scholarship confirms the command's contextual origin. No corroborating source outside fundamentalist circles maintains the problem is live.
narrative_ontology:disappearance_verdict(herem_command_dt7__contextual_supersession_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__contextual_supersession_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__contextual_supersession_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(herem_command_dt7__contextual_supersession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__contextual_supersession_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__contextual_supersession_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(herem_command_dt7__contextual_supersession_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(herem_command_dt7__contextual_supersession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint removes major life choices (marriage, association, religious liberty) from members without their consent and without a coordination function that benefits them. Suppression (0.78) is high because exit is punished by total social death (shunning, loss of family, eternal damnation narratives). Theater ratio (0.42) is moderate: the enforcement performs scriptural fidelity, but a growing share of energy defends the boundary itself rather than any living coordination need. Accessibility collapse (0.74) is high inside the community — alternatives are rhetorically and socially sealed. Resistance (0.52) is moderate: some members leave, some push for reinterpretation, but the cost is severe. The measurement series shows rising extraction and suppression over the interval, consistent with fundamentalist retrenchment amid secularization.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (fundamentalist leaders) experiences the constraint as a rope: it coordinates group identity and preserves the community against assimilation. The payer seats (members, outsiders) experience it as a snare: the coordination story is cover for extraction of labor, conformity, and reproductive futures. The engine computes this divergence from the structural data — the authored claim does not adjudicate it. The contextual_supersession_reading itself occupies an analytical observer seat: it sees the full structure and declares the constraint illegitimate.
 *
 * DIRECTIONALITY LOGIC:
 *   Fundamentalist leaders are structural beneficiaries (d ≈ 0.1): they collect authority, cohesion, and resource control from the enforcement. Fundamentalist members are targets (d ≈ 0.85): they bear the costs of restricted marriage markets, enforced conformity, and exit penalties; their exit options are 'identity_locked' (leaving means losing the self-concept constituted by the community). Outsiders excluded are also targets (d ≈ 0.9): they bear stigma and exclusion without any voice. Mainstream believers and scholars are observers (d ≈ 0.5): they experience neither extraction nor benefit from this specific enforcement. The derivation chain uses beneficiary/victim declarations plus exit options: leaders have arbitrage-grade exit (they could moderate and retain status), members are identity_locked, outsiders are trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting a nascent covenant community's ritual purity in a polytheistic environment) is dead — the historical conditions are gone. The arrangement persists because fundamentalist leaders extract authority from maintaining the boundary, and because the identity_locked exit option prevents members from forcing revision. The mandatrophy is resolved in the reading's own terms (the command is superseded), but unresolved in the fundamentalist commitment system that still enforces it. This mismatch — dead founding problem, live enforcement — is the signature of mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_identity,
    'This constraint is the contextual_supersession_reading of kernel herem_command_dt7. What structural changes would the sibling readings (allegorical_displacement_reading, durable_separation_reading) produce in the constraint''s beneficiary/victim structure and extraction profile?',
    'Author the sibling constraint stories separately and compare their base_properties and stakeholder structures. The engine''s constraint family analysis will reveal the structural deltas.',
    'If the sibling readings produce substantially different extraction profiles for the same referent arrangement, the kernel is confirmed as a site of genuine interpretive contest rather than a single constraint with measurement noise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Commitment-system framing: this story is one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by fundamentalist_members structural (excommunication, shunning, legal penalties in theocratic jurisdictions) or internalized (belief that leaving endangers salvation, identity fusion with the community)?',
    'Post-exit trajectory study: track suppression levels for members who leave fundamentalist communities. If suppression persists after exit, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression with them after exit, affecting χ for the payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in interpersonal/intercommunal constraint.').

omega_variable(
    ethnicity_vs_belief_boundary,
    'Does the residual enforcement actually operate on ethnic lines (descent from Canaanite nations) or has it functionally relocated to religious belief/consent boundaries (baptism, confession, community membership)?',
    'Ethnographic audit of fundamentalist marriage and exclusion practices: examine whether genetic ancestry or confessional status is the operative criterion.',
    'If relocated to belief/consent, the extraction profile shifts from ethnic snare to ideological snare — different victim set, different coalition possibilities for resistance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ethnicity_vs_belief_boundary, empirical, 'Whether the constraint''s boundary mechanism remains ethnic or has shifted to confessional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__contextual_supersession_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(herem_ctx_sup_tr_t0, herem_command_dt7__contextual_supersession_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(herem_ctx_sup_tr_t0, observed).
narrative_ontology:measurement(herem_ctx_sup_tr_t33, herem_command_dt7__contextual_supersession_reading, theater_ratio, 33, 0.32).
narrative_ontology:measurement_basis(herem_ctx_sup_tr_t33, observed).
narrative_ontology:measurement(herem_ctx_sup_tr_t66, herem_command_dt7__contextual_supersession_reading, theater_ratio, 66, 0.38).
narrative_ontology:measurement_basis(herem_ctx_sup_tr_t66, observed).
narrative_ontology:measurement(herem_ctx_sup_tr_t100, herem_command_dt7__contextual_supersession_reading, theater_ratio, 100, 0.42).
narrative_ontology:measurement_basis(herem_ctx_sup_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(herem_ctx_sup_be_t0, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(herem_ctx_sup_be_t0, observed).
narrative_ontology:measurement(herem_ctx_sup_be_t33, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 33, 0.62).
narrative_ontology:measurement_basis(herem_ctx_sup_be_t33, observed).
narrative_ontology:measurement(herem_ctx_sup_be_t66, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 66, 0.68).
narrative_ontology:measurement_basis(herem_ctx_sup_be_t66, observed).
narrative_ontology:measurement(herem_ctx_sup_be_t100, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 100, 0.72).
narrative_ontology:measurement_basis(herem_ctx_sup_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(herem_ctx_sup_su_t0, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(herem_ctx_sup_su_t0, observed).
narrative_ontology:measurement(herem_ctx_sup_su_t33, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 33, 0.71).
narrative_ontology:measurement_basis(herem_ctx_sup_su_t33, observed).
narrative_ontology:measurement(herem_ctx_sup_su_t66, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 66, 0.75).
narrative_ontology:measurement_basis(herem_ctx_sup_su_t66, observed).
narrative_ontology:measurement(herem_ctx_sup_su_t100, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 100, 0.78).
narrative_ontology:measurement_basis(herem_ctx_sup_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__contextual_supersession_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(herem_command_dt7__contextual_supersession_reading, 0.08).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__allegorical_displacement_reading).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__durable_separation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: herem_command_dt7. This reading (contextual_supersession) claims the kernel's directive is historically bounded and superseded, yielding low extractiveness in mainstream communities but a residual snare in fundamentalist enclaves. The allegorical_displacement reading relocates the constraint to internal moral warfare (different victim set, lower extraction). The durable_separation reading treats the kernel as a timeless mountain/rope (high extractiveness, broad victim set). The three stories share the kernel but instantiate structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(herem_command_dt7__contextual_supersession_reading, institutional, 0.12).
constraint_indexing:directionality_override(herem_command_dt7__contextual_supersession_reading, moderate, 0.82).
constraint_indexing:directionality_override(herem_command_dt7__contextual_supersession_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
