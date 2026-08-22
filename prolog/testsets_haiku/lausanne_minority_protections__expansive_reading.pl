% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__expansive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__expansive_reading, []).

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
 *   constraint_id: lausanne_minority_protections__expansive_reading
 *   human_readable: Lausanne Treaty Minority Protections (Expansive Reading)
 *   domain: international_law/religious_governance
 *
 * SUMMARY:
 *   The Lausanne Treaty (1923) included minority protections clauses that the
 *   expansive reading interprets as guaranteeing minority religious
 *   institutions functional autonomy over governance, property, and
 *   theological education — not merely individual worship rights. This
 *   reading grounds minority institutional survival and cultural continuity
 *   in treaty obligations that Turkish state law cannot unilaterally
 *   override. The expansive reading competes with two sibling readings: the
 *   restrictive reading (protections apply only to individual worship,
 *   institutional matters are domestic) and the guarantor reading
 *   (protections are enforceable through international supervision, not
 *   merely Turkish interpretation). This constraint story models the
 *   expansive reading as a moderate coordination rope: it solves the
 *   post-1923 problem of minority-majority coexistence through institutional
 *   autonomy, depends on active treaty compliance, and carries vulnerability
 *   if the interpretation contest is lost.
 *
 * KEY AGENTS:
 *   - Recognized minority institutions (Greek Orthodox, Armenian Apostolic, Jewish communities) — primary beneficiaries, depend on treaty reading for institutional continuity
 *   - Turkish state authority — agenda setter, balances treaty compliance against sovereignty assertions
 *   - European guarantor states — observers with potential enforcement capacity through human rights mechanisms
 *   - Minority community members — individual beneficiaries, gain collective religious practice rights through institutional protections
 *   - Theological education apparatus — identity-locked beneficiary, cannot function under state control without losing religious formation function
 *   - Nationalist constituencies — excluded voices that would advocate the restrictive reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__expansive_reading, 0.22).
domain_priors:suppression_score(lausanne_minority_protections__expansive_reading, 0.58).
domain_priors:theater_ratio(lausanne_minority_protections__expansive_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__expansive_reading, rope).
narrative_ontology:human_readable(lausanne_minority_protections__expansive_reading, "Lausanne Treaty Minority Protections (Expansive Reading)").
narrative_ontology:topic_domain(lausanne_minority_protections__expansive_reading, "international_law/religious_governance").

domain_priors:requires_active_enforcement(lausanne_minority_protections__expansive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__expansive_reading, '67b76fe1-838d-4ada-b57a-544148150a7f').
narrative_ontology:cs_kernel_codification('67b76fe1-838d-4ada-b57a-544148150a7f', fixed_text).
narrative_ontology:cs_authority_grounding('67b76fe1-838d-4ada-b57a-544148150a7f', lineage).
narrative_ontology:cs_interpretation_layer_present('67b76fe1-838d-4ada-b57a-544148150a7f').
narrative_ontology:cs_reading_relation('67b76fe1-838d-4ada-b57a-544148150a7f', lausanne_minority_protections__restrictive_reading, forecloses).
narrative_ontology:cs_reading_relation('67b76fe1-838d-4ada-b57a-544148150a7f', lausanne_minority_protections__guarantor_reading, coexists_with).
narrative_ontology:cs_axiom('67b76fe1-838d-4ada-b57a-544148150a7f', foundational, institutional_autonomy_guaranteed_by_treaty).
narrative_ontology:cs_axiom_status(institutional_autonomy_guaranteed_by_treaty, holdable).
narrative_ontology:cs_axiom_grounding('67b76fe1-838d-4ada-b57a-544148150a7f', institutional_autonomy_guaranteed_by_treaty, conventional).
narrative_ontology:cs_axiom('67b76fe1-838d-4ada-b57a-544148150a7f', foundational, minority_institutions_self_governing_not_subordinate).
narrative_ontology:cs_axiom_status(minority_institutions_self_governing_not_subordinate, holdable).
narrative_ontology:cs_axiom_grounding('67b76fe1-838d-4ada-b57a-544148150a7f', minority_institutions_self_governing_not_subordinate, deontological).
narrative_ontology:cs_reference_frame('67b76fe1-838d-4ada-b57a-544148150a7f', institutional_autonomy_guarantee_frame).
narrative_ontology:cs_drift_state('67b76fe1-838d-4ada-b57a-544148150a7f', contemporary_nationalist_pressure_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('67b76fe1-838d-4ada-b57a-544148150a7f', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__expansive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, recognized_minority_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, minority_community_members).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, theological_education_apparatus).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, treaty_based_minority_rights_doctrine).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, institutional_self_governance_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Christian minority religious institutions (Greek Orthodox, Armenian Apostolic, Jewish communities) retain formal rights under Lausanne to govern their internal affairs, maintain theological schools, and hold property. They depend entirely on treaty interpretation and state compliance for these protections to function. Their institutional continuity as autonomous bodies depends on this reading's acceptance by Turkish authorities.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, recognized_minority_institutions, beneficiary,
    moderate, generational, constrained, national).

% Holds primary authority to interpret and implement Lausanne obligations domestically. Can recognize minority institutions' self-governance claims or subordinate them to general Turkish law. Manages the balance between respecting treaty commitments and asserting state sovereignty over domestic governance structures.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, turkish_state_authority, agenda_setter,
    institutional, generational, mobile, national).

% Hold standing to invoke Lausanne guarantor roles, particularly through European Court of Human Rights proceedings and diplomatic channels. Their capacity to monitor and invoke remedies depends on which reading of Lausanne protections prevails. Observer seat: not directly governing Turkish implementation but potentially influential on interpretation contest.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, european_guarantor_states, observer,
    institutional, generational, analytical, continental).

% Individual members of minority faith communities gain substantive rights to congregate, worship, and access religious education only insofar as their institutions retain autonomous legal standing. Their ability to practice faith collectively, attend theological schools, or participate in institutional governance depends on institutional protections holding.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, minority_community_members, beneficiary,
    powerless, biographical, constrained, national).

% Political movements that view broad minority institutional autonomy as inconsistent with Turkish nation-building and secular governance would advocate for the restrictive reading. They are excluded from the beneficiary set but would object if institutional self-governance expanded; their voice shapes the political pressure against this reading's implementation.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, nationalist_constituencies, excluded,
    organized, biographical, mobile, national).

% Seminaries and theological schools operated by minority institutions depend on Lausanne protections to govern their curriculum, faculty hiring, and admission standards without state control. Their institutional identity fuses with religious authority — they cannot function under general state educational law without losing the religious formation function they exist to provide.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, theological_education_apparatus, beneficiary,
    moderate, generational, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__expansive_reading, diffuse).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__expansive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a functional interface between international treaty law and domestic religious governance: minority institutions retain self-administration over theological education, property, and internal affairs as a traded arrangement that satisfied minority security concerns in 1923 and persists as the coordinating mechanism for minority-state coexistence.
% TRANSFER_FUNCTION: Transfers authority and autonomy to minority institutions (no extraction; rather, allocation of regulatory rights). The constraint moves decision-making power from state authorities to institutional bodies over internal governance, property disposition, and clergy formation.
% ABSENT_VOICES: The restrictive reading (domestic-law supremacy advocates) is structurally excluded from the beneficiary set and would object vigorously; they would argue the constraint is obsolete and sovereignty-impairing. Guarantor state representatives do not directly govern but can intervene via European human rights enforcement, creating an implicit power over interpretation that minority institutions cannot match.
% DISAPPEARANCE_RATIONALE: If Lausanne protections for institutional autonomy vanished, minority religious institutions would immediately lose legal standing to govern themselves; theology schools would either dissolve or be absorbed into state control; property disputes would shift to state courts under general law; minority religious authority structures would collapse into individuals exercising only personal worship rights within state-regulated parameters.
% FOUNDING_PROBLEM: Post-WWI Ottoman successor state needed to guarantee minority security and cultural survival while establishing Turkish state authority: Lausanne Treaty's minority clauses traded international recognition and sovereignty for binding protections of minority institutional autonomy, property, and religious authority structures.
% FOUNDING_PROBLEM_CORROBORATION: Turkish authorities now argue the founding problem is resolved (modern minorities enjoy general citizenship rights, separate protections are outdated). Minority institutions and European human rights bodies argue the founding problem persists — institutional autonomy remains the only reliable protection against majoritarian pressure to subordinate minority governance to state law. Historians external to both reading communities attest the 1923 founding problem was real and severe (documented persecution, forced assimilation pressure); contemporary contestation is whether that problem's conditions still obtain.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__expansive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__expansive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__expansive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lausanne_minority_protections__expansive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__expansive_reading, 0.22, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__expansive_reading_tests).
:- end_tests(lausanne_minority_protections__expansive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low to moderate (0.22 final) because this reading produces no systematic extraction — minority institutions gain autonomy without bearing costs asymmetric to benefited parties. Suppression is moderate (0.58) because the constraint's persistence depends on active Turkish state compliance with treaty interpretation and on active defense against the restrictive reading's pressure. Theater is moderate-high (0.41–0.42) because enforcement activity increasingly shifts away from genuine institutional governance toward performative compliance — state-minority institutions negotiations that follow formal protocols but where substantive autonomy erodes. The measurement series shows suppression intensifying over the interval (0.48→0.61 at midpoint) reflecting rising nationalist pressure against expansive institutional autonomy, then moderating slightly at the endpoint reflecting European diplomatic counter-pressure. Theater rises throughout (0.28→0.42), modeling increasing proportion of minority-state engagement becoming ritual compliance rather than substantive self-governance.
 *
 * PERSPECTIVAL GAP:
 *   The payer/beneficiary asymmetry is not monetary but interpretive: Turkish authorities bear the 'cost' of limiting sovereignty; minority institutions bear the cost of depending on external legal infrastructure (Lausanne treaty text, European courts) for protections they cannot self-enforce. The theater ratio rises because increasing minority-state negotiations follow Lausanne protocols while actual substantive autonomy contracts — enforcement becomes more performative, less functional.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority institutions are beneficiaries structurally (gain autonomy rights, bear no extraction cost). But directionality computation must account for exit asymmetry: if the expansive reading loses the interpretation contest, minority institutions have no exit — they cannot abandon the Lausanne framework and retain institutional standing. Their 'constrained' exit option (cannot leave without ceasing to exist as treaty-protected entities) drives effective directionality toward target status despite beneficiary role. Turkish state authority is the agenda-setter (controls interpretation, sets enforcement posture) with mobile exit (can shift to restrictive reading without organizational dissolution). This asymmetry — beneficiary with trapped identity vs. powerful agenda-setter with mobile exit — is the constraint's structural fragility.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not mandatrophy-resolved, but it carries high mandatrophy risk. The founding problem (post-WWI minority security) was acute in 1923; contemporary Turkish and minority stakeholders contest whether it persists. If Turkish authorities were to argue 'the founding problem (persecution pressure, forced assimilation) is solved by modern citizenship law, so Lausanne institutional protections are obsolete,' that would trigger mandatrophy classification. The expansive reading's survival depends on maintaining that the founding problem's conditions still obtain or that institutional autonomy is a value beyond the original problem. The measurement series showing rising suppression (enforcement effort intensifying to defend against restrictive reading) and rising theater (ritual compliance replacing substantive governance) traces the pathway toward potential mandatrophy: the constraint persists as performance even as its functional justification weakens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_autonomy_scope_ambiguity,
    'Does ''institutional self-administration'' under Lausanne include property ownership, theological school autonomy, and internal governance independence, or extend only to religious practice within state-supervised structures?',
    'Comparative treaty-law analysis of Article 37-44 language and original drafting intent; precedent from European Court of Human Rights and Turkish Constitutional Court cases interpreting scope.',
    'Narrow reading collapses into restrictive_reading constraint (property/schools subject to general law); broad reading sustains this expansive reading. Classification hinges on interpretation of a single contested article.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_autonomy_scope_ambiguity, conceptual, 'Textual ambiguity in Treaty language on institutional autonomy scope.').

omega_variable(
    founding_problem_obsolescence,
    'Has the founding problem (minority security against majoritarian state pressure, forced assimilation) been structurally resolved by modern constitutional citizenship rights, or do minority institutions remain vulnerable to majoritarian restrictions without Lausanne-based institutional autonomy?',
    'Historical analysis of minority institution pressures since 1923 (school closures, property expropriations, state restrictions on theological education); comparative analysis of minority status in democracies without treaty-based institutional protections.',
    'If founding problem is dead (modern rights sufficient), the constraint slides toward Piton classification — persists as ritual but without functional justification. If founding problem is live, the rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the constraint''s original justification conditions still obtain.').

omega_variable(
    treaty_supremacy_vs_sovereignty,
    'Can Turkey legitimately interpret Lausanne protections as subject to evolutionary reading under modern Turkish law, or are minority institutions entitled to a frozen understanding of 1923 institutional autonomy?',
    'Vienna Convention on Law of Treaties provisions on evolution vs. original intent; comparative precedent from other nations interpreting minority-protection treaties; European Court review of Turkish constitutional theory.',
    'Treaty-evolution framing favors restrictive reading (modern law can reinterpret protections); original-intent framing favors expansive reading. The resolution mechanism determines which sibling reading gains legitimacy in interpretive contests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(treaty_supremacy_vs_sovereignty, conceptual, 'Whether treaty protections are fixed at 1923 meaning or subject to evolutionary interpretation.').

omega_variable(
    identity_lock_persistence,
    'For theological education apparatus and minority community members, is the identity-lock (cannot exit without community dissolution) permanent, or can it be dissolved through secular alternative education paths?',
    'Generational study of minority youth educational choices; sociological research on whether theological school closure leads to community identity dissolution or adaptation to secular education.',
    'If identity-lock dissolves (minorities adapt to secular education), minority institutions'' exit options improve from ''trapped'' to ''constrained,'' increasing their structural power relative to state authority. Would change directionality calculus and potentially shift classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Whether minority community members'' institutional identity is permanently fused with theological education.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the rising suppression requirement driven by structural barriers (legal restrictions, school closures, property disputes) or internalized (minority institutions voluntarily accept constraints to avoid majoritarian backlash)?',
    'Post-constraint analysis: if minority institutions faced sudden removal of legal barriers, would suppression persist? Historical study of Turkish administrative pressure vs. minority strategic accommodation.',
    'If suppression is structural, it indicates active enforcement machinery hardening over the interval (rope persists, may shift to snare). If internalized, minority institutions carry the constraint with them even if legal barriers fall — deeper suppression than measurement suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether measured suppression is structural legal barriers or internalized minority accommodation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__expansive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t0, lausanne_minority_protections__expansive_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(laus_tr_t0, projected).
narrative_ontology:measurement(laus_tr_t15, lausanne_minority_protections__expansive_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement_basis(laus_tr_t15, observed).
narrative_ontology:measurement(laus_tr_t30, lausanne_minority_protections__expansive_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement_basis(laus_tr_t30, observed).
narrative_ontology:measurement(laus_tr_t50, lausanne_minority_protections__expansive_reading, theater_ratio, 50, 0.39).
narrative_ontology:measurement_basis(laus_tr_t50, observed).
narrative_ontology:measurement(laus_tr_t75, lausanne_minority_protections__expansive_reading, theater_ratio, 75, 0.42).
narrative_ontology:measurement_basis(laus_tr_t75, observed).
narrative_ontology:measurement(laus_tr_t100, lausanne_minority_protections__expansive_reading, theater_ratio, 100, 0.41).
narrative_ontology:measurement_basis(laus_tr_t100, projected).

% Extraction over time
narrative_ontology:measurement(laus_be_t0, lausanne_minority_protections__expansive_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(laus_be_t0, projected).
narrative_ontology:measurement(laus_be_t15, lausanne_minority_protections__expansive_reading, base_extractiveness, 15, 0.16).
narrative_ontology:measurement_basis(laus_be_t15, observed).
narrative_ontology:measurement(laus_be_t30, lausanne_minority_protections__expansive_reading, base_extractiveness, 30, 0.19).
narrative_ontology:measurement_basis(laus_be_t30, observed).
narrative_ontology:measurement(laus_be_t50, lausanne_minority_protections__expansive_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement_basis(laus_be_t50, observed).
narrative_ontology:measurement(laus_be_t75, lausanne_minority_protections__expansive_reading, base_extractiveness, 75, 0.25).
narrative_ontology:measurement_basis(laus_be_t75, observed).
narrative_ontology:measurement(laus_be_t100, lausanne_minority_protections__expansive_reading, base_extractiveness, 100, 0.22).
narrative_ontology:measurement_basis(laus_be_t100, projected).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t0, lausanne_minority_protections__expansive_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(laus_su_t0, projected).
narrative_ontology:measurement(laus_su_t15, lausanne_minority_protections__expansive_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement_basis(laus_su_t15, observed).
narrative_ontology:measurement(laus_su_t30, lausanne_minority_protections__expansive_reading, suppression_requirement, 30, 0.54).
narrative_ontology:measurement_basis(laus_su_t30, observed).
narrative_ontology:measurement(laus_su_t50, lausanne_minority_protections__expansive_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement_basis(laus_su_t50, observed).
narrative_ontology:measurement(laus_su_t75, lausanne_minority_protections__expansive_reading, suppression_requirement, 75, 0.61).
narrative_ontology:measurement_basis(laus_su_t75, observed).
narrative_ontology:measurement(laus_su_t100, lausanne_minority_protections__expansive_reading, suppression_requirement, 100, 0.58).
narrative_ontology:measurement_basis(laus_su_t100, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__expansive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__expansive_reading, 0.12).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-story Lausanne minority protections family. The expansive reading (this story) interprets institutional autonomy guarantees as functional and binding on Turkish law. The restrictive reading limits protections to individual worship. The guarantor reading accepts expansive protections but grounds them in international supervisory enforcement. The three readings share the same referent (Lausanne Treaty Article 37-44 protections) but produce different constraint structures due to different interpretations of institutional scope and enforcement mechanisms. The expansive and restricitive readings foreclose each other; each coexists with the guarantor reading as alternative enforcement framings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lausanne_minority_protections__expansive_reading, powerless, 0.68).
constraint_indexing:directionality_override(lausanne_minority_protections__expansive_reading, moderate, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
