% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__liturgical_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: hebrew_vitality__liturgical_reading
 *   human_readable: Liturgical Preservation as Vitality (Hebrew)
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This constraint story captures the liturgical reading of Hebrew vitality:
 *   the claim that unbroken ritual use of Hebrew in prayer, study, and
 *   ceremonial life constitutes genuine vitality, regardless of vernacular
 *   status. The reading treats the liturgical domain as a self-sufficient
 *   kernel — Hebrew lives because it is used in the synagogue, the beit
 *   midrash, and the lifecycle rituals that structure Jewish time. Rabbinic
 *   authorities and liturgical institutions are the primary beneficiaries,
 *   deriving authority, institutional continuity, and communal cohesion from
 *   the constraint's operation. No victim set is declared because the
 *   reading's domain restriction (ritual only) excludes the costs of
 *   vernacular acquisition; lay participants opt into liturgical Hebrew
 *   voluntarily within the religious frame. The claimed type is rope: a
 *   genuine coordination mechanism (shared liturgical language enabling
 *   cross-communal prayer, study, and identity) with minimal coercive
 *   overhead and no identified extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__liturgical_reading, 0.12).
domain_priors:suppression_score(hebrew_vitality__liturgical_reading, 0.18).
domain_priors:theater_ratio(hebrew_vitality__liturgical_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__liturgical_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__liturgical_reading, "Liturgical Preservation as Vitality (Hebrew)").
narrative_ontology:topic_domain(hebrew_vitality__liturgical_reading, "sociolinguistics/language_revitalization/jewish_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__liturgical_reading, '5dbc3ad1-289e-4681-a920-f4590ad2027c').
narrative_ontology:cs_kernel_codification('5dbc3ad1-289e-4681-a920-f4590ad2027c', fixed_text).
narrative_ontology:cs_authority_grounding('5dbc3ad1-289e-4681-a920-f4590ad2027c', lineage).
narrative_ontology:cs_interpretation_layer_present('5dbc3ad1-289e-4681-a920-f4590ad2027c').
narrative_ontology:cs_reading_relation('5dbc3ad1-289e-4681-a920-f4590ad2027c', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_reading_relation('5dbc3ad1-289e-4681-a920-f4590ad2027c', hebrew_vitality__hybrid_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('5dbc3ad1-289e-4681-a920-f4590ad2027c', foundational, liturgical_continuity_suffices_for_vitality).
narrative_ontology:cs_axiom_status(liturgical_continuity_suffices_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('5dbc3ad1-289e-4681-a920-f4590ad2027c', liturgical_continuity_suffices_for_vitality, deontological).
narrative_ontology:cs_axiom('5dbc3ad1-289e-4681-a920-f4590ad2027c', secondary, vernacular_use_not_required_for_vitality).
narrative_ontology:cs_axiom_status(vernacular_use_not_required_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('5dbc3ad1-289e-4681-a920-f4590ad2027c', vernacular_use_not_required_for_vitality, deontological).
narrative_ontology:cs_reference_frame('5dbc3ad1-289e-4681-a920-f4590ad2027c', unbroken_liturgical_chain).
narrative_ontology:cs_drift_state('5dbc3ad1-289e-4681-a920-f4590ad2027c', contemporary_vernacular_revival, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5dbc3ad1-289e-4681-a920-f4590ad2027c', '2026-08-03T14:22:11Z').
narrative_ontology:cs_kernel_id(hebrew_vitality__liturgical_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, liturgical_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, observant_lay_participants).
narrative_ontology:constraint_victim(hebrew_vitality__liturgical_reading, observant_lay_participants).
narrative_ontology:constraint_vindicates(hebrew_vitality__liturgical_reading, unbroken_liturgical_use_suffices_for_vitality).
narrative_ontology:constraint_vindicates(hebrew_vitality__liturgical_reading, ritual_continuity_preserves_national_identity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold interpretive authority over liturgical Hebrew: determine pronunciation standards, textual variants, and ritual correctness. Their authority derives from the constraint's operation — if liturgical Hebrew were abandoned or vernacularized, their specialized role would diminish. They can move between denominations, communities, or educational institutions while retaining liturgical authority (arbitrage exit). They do not bear the learning costs imposed on lay participants.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, rabbinic_authorities, beneficiary,
    institutional, generational, arbitrage, global).

% Synagogues, yeshivas, and religious courts whose institutional identity and funding depend on maintaining Hebrew liturgy. They coordinate communal practice, train ritual leaders, and preserve textual traditions. Their institutional continuity is bound to the constraint; they benefit from the coordination function (shared liturgy enables communal cohesion) and from the authority structure it sustains. Exit means institutional transformation or dissolution.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, liturgical_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Invest significant time in acquiring liturgical Hebrew literacy (prayer book fluency, Torah reading, textual study) to participate fully in communal ritual. They bear the learning cost but gain the coordination benefit: ability to pray in any synagogue worldwide, access to canonical texts, communal belonging. Exit is constrained — leaving the liturgical frame means leaving the religious community — but within the frame, participation is experienced as voluntary religious commitment, not extraction.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, observant_lay_participants, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__liturgical_reading, observant_lay_participants, beneficiary).

% Reform, Reconstructionist, and some Conservative communities that have substantially vernacularized liturgy while retaining Hebrew elements. They would object to the claim that Hebrew vitality requires unbroken liturgical use in its traditional form, arguing that vitality includes adaptation. They are excluded from the liturgical_reading's frame because that reading defines vitality in terms of traditional liturgical continuity, which their practice modifies. Their exit is mobile — they have built alternative liturgical frameworks.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, liberal_denomination_participants, excluded,
    organized, biographical, mobile, global).

% Analyze Hebrew's trajectory as a case study in language revitalization. They observe the constraint's operation across all three readings but do not participate in the liturgical frame as insiders. Their analytical frame treats the liturgical reading as one of several competing vitality criteria, not as the definition of vitality itself.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, sociolinguists_of_jewish_languages, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__liturgical_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_vitality__liturgical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared liturgical language enabling Jews across geographic, temporal, and denominational boundaries to pray together, study the same canonical texts, and maintain communal cohesion without a shared vernacular.
% TRANSFER_FUNCTION: Moves learning effort (time, education, cognitive load) from lay participants toward the maintenance of a shared liturgical standard; the coordination benefit (communal unity, textual access, identity continuity) flows back to all participants. No material transfer to rabbinic authorities beyond the status/authority inherent in the coordination role.
% ABSENT_VOICES: Secular Hebrew speakers (native_daily_reading proponents) who would argue that vitality requires living vernacular use, not ritual preservation. They are absent from the liturgical frame because that frame defines vitality in ritual terms, making vernacular absence irrelevant to its criterion. Also absent: historical communities that lost liturgical Hebrew (e.g., some medieval communities that shifted to Judeo-Arabic liturgy) — their disappearance is the counterfactual the reading treats as vitality loss.
% DISAPPEARANCE_RATIONALE: If the liturgical Hebrew constraint vanished overnight, diaspora Jewish communities would lose their primary cross-communal coordination mechanism: shared prayer texts, Torah reading cycles, and the textual substrate that enables halakhic discourse across boundaries. Communities would fragment into vernacular isolates, textual study would require translation infrastructures, and the rabbinic authority structure would lose its common language. The world would rearrange — new coordination mechanisms would need to be built or existing ones (English, local languages) would replace Hebrew in liturgy.
% FOUNDING_PROBLEM: Diaspora Jewish communities needed a shared language for prayer, study, and legal discourse to maintain unity across geographic dispersion and vernacular diversity, without a shared spoken language.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by historical sources outside the rabbinic tradition: Geniza documents showing cross-communal correspondence in Hebrew, responsa literature demonstrating halkhic coordination across diaspora, and contemporary sociolinguistic analysis (e.g., Joshua Fishman's work on Hebrew as a 'sanctified language' maintaining ethnic boundaries). The problem remains live because diaspora communities still lack a shared vernacular and still use Hebrew liturgy for coordination — though the native_daily_reading's success in Israel creates a new vernacular center that changes the coordination geometry.
narrative_ontology:disappearance_verdict(hebrew_vitality__liturgical_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__liturgical_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__liturgical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(hebrew_vitality__liturgical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__liturgical_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__liturgical_reading_tests).
:- end_tests(hebrew_vitality__liturgical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the constraint operates as a coordination standard: a shared liturgical language allows Jews across diaspora to pray together, study the same texts, and maintain textual continuity. The cost is learning liturgical Hebrew, but this cost is internal to the religious commitment — not extracted by an external party. Suppression is low (0.18) because alternatives (vernacular prayer, translated texts) exist and are used in liberal movements; the constraint persists through communal norm, not enforcement. Theater ratio (0.25) reflects that some liturgical performance is performative — maintained for continuity's sake rather than communicative function — but the core coordination function remains genuine. Accessibility collapse (0.72) is moderately high because once the liturgical frame is adopted, alternatives (praying in vernacular) feel like a loss of authenticity, but this is internal to the frame, not externally imposed. Resistance (0.15) is low because the constraint is largely self-sustaining within committed communities.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic seat experiences this as a mountain — an immutable feature of Jewish continuity that cannot be changed without losing authenticity. The lay seat experiences it as a rope — a coordination mechanism they voluntarily maintain because it works. The analytical seat (sociolinguist) sees a historically contingent arrangement that persisted because it solved a genuine coordination problem (diaspora unity) but now coexists with a vernacular revival that changes the constraint's function. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities and liturgical institutions sit at the beneficiary end (d ≈ 0.15): they gain authority, institutional purpose, and communal cohesion from the constraint's operation. Their exit options are arbitrage — they can shift between denominations, roles, or institutions while retaining liturgical authority. Lay participants sit near symmetric (d ≈ 0.5): they bear the learning cost but gain the coordination benefit (communal prayer, textual access, identity). Their exit is constrained — leaving the liturgical frame means leaving the community — but within the frame, participation is voluntary. No agent sits at the target end (d ≈ 1.0) because the reading's domain restriction excludes vernacular costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora coordination via shared liturgical language) remains live — Jewish communities still use Hebrew liturgy for cross-communal unity. However, the native_daily_reading's success in creating vernacular Hebrew introduces a mandatrophy question: does the liturgical constraint now serve a different function (identity boundary maintenance) than its founding one? The reading itself does not declare mandatrophy resolved; the founding problem persists but its urgency has shifted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does the liturgical reading instantiate a genuinely distinct constraint from the kernel''s other readings, or does it merely describe a different measurement basis for the same constraint?',
    'Test whether ε changes when the referent shifts from liturgical domain to vernacular daily life. If ε remains low and the beneficiary/victim structure is stable, the reading shares the kernel''s constraint; if ε rises and victims appear, they are distinct constraints.',
    'If distinct, this reading stands alone with its own classification; if shared, the kernel has a single constraint with multiple observational windows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading is a separate constraint or an observational window on the kernel''s single constraint.').

omega_variable(
    liturgical_beneficiary_nature,
    'Do rabbinic authorities benefit materially from the liturgical constraint, or is their benefit purely status/authority within the ritual frame?',
    'Trace resource flows: synagogue funding, educational appointments, communal leadership positions tied to liturgical authority. If material flows exist, the beneficiary declaration is structural; if only symbolic authority, the benefit may be internal to the commitment system.',
    'Material benefit would support tangled_rope classification if extraction from laity exists; symbolic benefit keeps this reading in rope territory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(liturgical_beneficiary_nature, empirical, 'Whether rabbinic beneficiary status involves material extraction or symbolic authority.').

omega_variable(
    vernacular_absence_as_victimlessness,
    'Is the absence of a victim set genuine, or does the liturgical reading''s domain restriction mask extraction that would appear if the referent included daily life?',
    'Compare resource demands (time, education, communal obligation) imposed on lay participants by liturgical Hebrew versus the native_daily_reading''s demands. If liturgical participation imposes costs without vernacular payoff, victims exist but are domain-restricted.',
    'Hidden victims would reclassify toward tangled_rope or snare; confirmed victimlessness supports rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vernacular_absence_as_victimlessness, empirical, 'Whether domain restriction conceals extractive costs on lay participants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__liturgical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_vitality__liturgical_reading_tr_t0, hebrew_vitality__liturgical_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(hebrew_vitality__liturgical_reading_tr_t0, observed).
narrative_ontology:measurement(hebrew_vitality__liturgical_reading_tr_t50, hebrew_vitality__liturgical_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement_basis(hebrew_vitality__liturgical_reading_tr_t50, observed).
narrative_ontology:measurement(hebrew_vitality__liturgical_reading_tr_t100, hebrew_vitality__liturgical_reading, theater_ratio, 100, 0.25).
narrative_ontology:measurement_basis(hebrew_vitality__liturgical_reading_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(hebrew_vitality__liturgical_reading_be_t0, hebrew_vitality__liturgical_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(hebrew_vitality__liturgical_reading_be_t0, observed).
narrative_ontology:measurement(hebrew_vitality__liturgical_reading_be_t50, hebrew_vitality__liturgical_reading, base_extractiveness, 50, 0.1).
narrative_ontology:measurement_basis(hebrew_vitality__liturgical_reading_be_t50, observed).
narrative_ontology:measurement(hebrew_vitality__liturgical_reading_be_t100, hebrew_vitality__liturgical_reading, base_extractiveness, 100, 0.12).
narrative_ontology:measurement_basis(hebrew_vitality__liturgical_reading_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebrew_vitality__liturgical_reading_su_t0, hebrew_vitality__liturgical_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(hebrew_vitality__liturgical_reading_su_t0, observed).
narrative_ontology:measurement(hebrew_vitality__liturgical_reading_su_t50, hebrew_vitality__liturgical_reading, suppression_requirement, 50, 0.15).
narrative_ontology:measurement_basis(hebrew_vitality__liturgical_reading_su_t50, observed).
narrative_ontology:measurement(hebrew_vitality__liturgical_reading_su_t100, hebrew_vitality__liturgical_reading, suppression_requirement, 100, 0.18).
narrative_ontology:measurement_basis(hebrew_vitality__liturgical_reading_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__liturgical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_vitality__liturgical_reading, 0.08).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__native_daily_reading).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% This reading and its siblings decompose the kernel 'hebrew_vitality' per the ε-invariance principle: each reading assigns a different ε to a different referent (liturgical domain vs. vernacular daily life vs. combined trajectory). The liturgical reading's ε (0.12) reflects ritual-domain coordination only. The native daily reading's ε would be higher (vernacular acquisition costs). The hybrid reading's ε would be intermediate (both domains, transitional). They are linked via affects_constraints because the liturgical substrate historically enabled the vernacular revival, and the vernacular revival now reshapes the liturgical constraint's function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_vitality__liturgical_reading, institutional, 0.15).
constraint_indexing:directionality_override(hebrew_vitality__liturgical_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
