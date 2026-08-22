% ============================================================================
% CONSTRAINT STORY: anthropological_record__indigenous_epistemology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__indigenous_epistemology_reading, []).

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
 *   constraint_id: anthropological_record__indigenous_epistemology_reading
 *   human_readable: Oral Tradition Authority Over Anthropological Record (Indigenous Epistemology Reading)
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   The anthropological record — the aggregate of material evidence about
 *   human origins, migration, and ancestry — is subject to competing readings
 *   grounded in different epistemological frameworks. The
 *   indigenous-epistemology reading claims that oral tradition, transmitted
 *   across generations within indigenous communities, is a legitimate and
 *   primary source of knowledge about ancestral continuity and
 *   place-relationships. This reading does not argue that material evidence
 *   is false; rather, it asserts that material evidence is insufficient
 *   without oral tradition, and that communities have epistemic authority to
 *   interpret evidence through their own frameworks. Under this reading,
 *   external researchers (academic anthropologists, museum curators,
 *   credentialed scientists) are payers: they must subordinate their analytic
 *   authority to community epistemology, accept interpretive constraints, and
 *   bear the cost of research access negotiated under terms communities set.
 *   This is a tangled-rope structure: genuine coordination function (keeping
 *   ancestral knowledge alive across generations) paired with asymmetric
 *   extraction (communities gain interpretive authority; external
 *   institutions lose unilateral control). The claim/metric gap is
 *   deliberate: the constraint is claimed as tangled-rope while metrics show
 *   substantial suppression (0.71 at interval end) and moderately high
 *   theater (0.41), indicating a shift toward snare-adjacent dynamics as
 *   institutional resistance hardens.
 *
 * KEY AGENTS:
 *   - indigenous_communities — moderate power, civilizational horizon, identity-locked to oral tradition; set terms, control narratives, benefit from epistemic authority
 *   - oral_tradition_knowledge_keepers — moderate power, civilizational horizon, identity-locked; hold non-fungible transmitted knowledge; exit means cultural transmission cessation
 *   - external_researchers — institutional power, biographical horizon, constrained exit; must negotiate research access, accept interpretive subordination, bear cost of community-set terms
 *   - institutions_of_credentialed_science — institutional power, generational horizon, partially mobile; lose unilateral interpretive authority, fund research under community-set terms, repatriate remains
 *   - settler_state_institutions — institutional power, generational horizon, analytical position; mediate enforcement via law and policy (NAGPRA, consultation requirements, repatriation mandates)
 *   - creationist_communities — powerful, civilizational horizon, trapped exit (their frameworks are subordinated); excluded from setting research terms unless aligned with oral tradition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__indigenous_epistemology_reading, 0.62).
domain_priors:suppression_score(anthropological_record__indigenous_epistemology_reading, 0.71).
domain_priors:theater_ratio(anthropological_record__indigenous_epistemology_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__indigenous_epistemology_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__indigenous_epistemology_reading, "Oral Tradition Authority Over Anthropological Record (Indigenous Epistemology Reading)").
narrative_ontology:topic_domain(anthropological_record__indigenous_epistemology_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__indigenous_epistemology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__indigenous_epistemology_reading, '1ec86325-b373-4700-ac12-5ac562e558f6').
narrative_ontology:cs_kernel_codification('1ec86325-b373-4700-ac12-5ac562e558f6', distributed).
narrative_ontology:cs_authority_grounding('1ec86325-b373-4700-ac12-5ac562e558f6', distributed).
narrative_ontology:cs_reading_relation('1ec86325-b373-4700-ac12-5ac562e558f6', anthropological_record__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ec86325-b373-4700-ac12-5ac562e558f6', anthropological_record__creationist_reading, coexists_with).
narrative_ontology:cs_axiom('1ec86325-b373-4700-ac12-5ac562e558f6', foundational, oral_tradition_epistemically_primary).
narrative_ontology:cs_axiom_status(oral_tradition_epistemically_primary, holdable).
narrative_ontology:cs_axiom_grounding('1ec86325-b373-4700-ac12-5ac562e558f6', oral_tradition_epistemically_primary, conventional).
narrative_ontology:cs_axiom('1ec86325-b373-4700-ac12-5ac562e558f6', foundational, community_authority_over_ancestral_interpretation).
narrative_ontology:cs_axiom_status(community_authority_over_ancestral_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('1ec86325-b373-4700-ac12-5ac562e558f6', community_authority_over_ancestral_interpretation, deontological).
narrative_ontology:cs_reference_frame('1ec86325-b373-4700-ac12-5ac562e558f6', community_stewardship_of_knowledge).
narrative_ontology:cs_drift_state('1ec86325-b373-4700-ac12-5ac562e558f6', contemporary_institutional_pushback, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1ec86325-b373-4700-ac12-5ac562e558f6', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(anthropological_record__indigenous_epistemology_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, indigenous_communities).
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, oral_tradition_knowledge_keepers).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, external_researchers).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, institutions_of_credentialed_science).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stewards of ancestral knowledge held in oral tradition spanning generations. Under this reading, they possess epistemic authority to interpret the anthropological record through the lens of relational continuity with ancestors and place. They set terms for research access, control narrative authority over remains and artifacts, and legitimate which interpretations align with oral tradition. Enforcement rests on community consensus about knowledge transmission and ancestor relations.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, indigenous_communities, beneficiary,
    moderate, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(anthropological_record__indigenous_epistemology_reading, indigenous_communities, agenda_setter).

% Hold the transmitted knowledge of lineage, place-connection, and ancestral continuity. This reading legitimates their authority as primary interpreters of evidence touching their communities' origins and relationships to land. Their knowledge is non-fungible and cannot be replaced by external expertise. Exit means cultural cessation of transmission, which dissolves the very epistemic ground the reading rests on.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, oral_tradition_knowledge_keepers, beneficiary,
    moderate, civilizational, identity_locked, regional).

% Academics and researchers from outside the indigenous community seeking to study ancestral records, remains, and origins narratives. Under this reading, they must subordinate their analytic frameworks (scientific method, textual authority, credentialed expertise) to community epistemology. They bear the cost of renegotiating research terms, accepting interpretive constraints, and potentially having their findings rejected if they contradict oral tradition. Their exit is constrained: abandoning the research means losing access to irreplaceable communities and archives.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, external_researchers, payer,
    institutional, biographical, constrained, global).

% Universities, museums, archaeological and anthropological bodies that claim epistemic authority over methods of interpreting material evidence and human origins. This reading subordinates their authority to community epistemology and constrains what knowledge they can legitimately extract from remains in indigenous territories. They pay by losing unilateral interpretive control, by funding research under terms they did not set, and by repatriating remains when communities invoke oral tradition claims. Their exit is partial: they can redirect research elsewhere, but do so at reputational cost.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, institutions_of_credentialed_science, payer,
    institutional, generational, mobile, global).

% Government agencies, courts, and regulatory bodies that mediate between indigenous communities and credentialed institutions. They operate under this reading when they recognize indigenous oral tradition as a basis for repatriation claims, land rights, and research governance. Their analytical seat means they can alter the constraint's enforcement by changing law (NAGPRA-style frameworks, consultation requirements) or by reversing recognition.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, settler_state_institutions, observer,
    institutional, generational, analytical, national).

% Religious communities and institutions that interpret the record through divine creation or scriptural frameworks. This reading excludes them from legitimate authority over interpretation, treating their frameworks as alternative cosmologies rather than epistemically coordinate with oral tradition. They are barred from setting research terms in indigenous territories unless they align with oral tradition epistemology.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, creationist_communities, excluded,
    powerful, civilizational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__indigenous_epistemology_reading, indigenous_communities).
narrative_ontology:fixing_cost_class(anthropological_record__indigenous_epistemology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates multi-generational knowledge transmission about ancestral lineage and place-relationship by centering oral tradition as epistemic authority. Solves the collective-action problem of keeping ancestral knowledge alive against pressure from external frameworks that would atomize or reinterpret it. Ensures communities maintain interpretive control over their own origins narratives rather than having them mediated through external credentialed systems.
% TRANSFER_FUNCTION: Transfers epistemic authority (the right to define what counts as valid knowledge about the record) from external researchers and credentialed institutions to indigenous communities. Moves material artifacts (remains, objects) from museum custody to community stewardship. Extracts research access and interpretive deference from external institutions as the price of working with indigenous communities and materials.
% ABSENT_VOICES: External researchers who believe material evidence should be interpreted through scientific method independent of community frameworks; credentialed scientists who claim expertise should guide interpretation; museums and universities that see themselves as proper custodians of human remains. These parties would argue for open scientific access, universal methods, and institutional rather than communal authority — but are subordinated or excluded under this reading's rules.
% DISAPPEARANCE_RATIONALE: If this reading vanished and external credentialed institutions regained unilateral authority, communities would lose control over remains and narratives; museums would reassert curatorial rights; research would proceed under scientific method alone; repatriation claims would weaken; and generations of knowledge transmission could be disrupted or overwritten by external interpretation. The social ecology of who speaks authoritatively about origins would reorganize around credentialed expertise rather than community epistemology.
% FOUNDING_PROBLEM: Colonial science denied indigenous knowledge systems as legitimate interpretation, looted ancestral remains, and imposed external frameworks (evolution, taxonomy, museum narratives) as the only valid reading of the record. Communities needed epistemic authority to recover and protect their own knowledge traditions.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous scholars, anthropologists from within indigenous communities, and institutional ethics boards cite ongoing disputes over remains repatriation, research protocols, and interpretive authority as evidence the problem persists. External credentialed institutions acknowledge (in policy if not practice) the historical denial but contest whether oral tradition epistemology should override scientific method in all contexts. The problem's status is corroborated by legal settlements (NAGPRA repatriations), decolonial scholarship, and museum policy shifts — but remains contested by institutions reluctant to surrender unilateral interpretive authority.
narrative_ontology:disappearance_verdict(anthropological_record__indigenous_epistemology_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__indigenous_epistemology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__indigenous_epistemology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(anthropological_record__indigenous_epistemology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__indigenous_epistemology_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__indigenous_epistemology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__indigenous_epistemology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__indigenous_epistemology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.62 over 25 years and plateaus, modeling the reading's consolidation through repatriation law, institutional policy adoption, and shifting norms around research ethics. Theater rises from 0.25 to 0.41 over the same span: institutions increasingly perform deference to oral tradition in public while resisting it in practice (museums claim to honor oral tradition while negotiating narrowly over remains; researchers frame projects as 'collaborative' while maintaining control over interpretation). Suppression rises from 0.55 to 0.71, indicating the reading's persistence depends increasingly on active enforcement (legal mandates, institutional sanctions against non-compliant research, community-led exclusion of researchers who treat oral tradition as negotiable). Resistance at 0.58 reflects external pushback: credentialed institutions contest the reading's authority, creationist communities reject its subordination, and researchers seek exceptions. The plateau at t=25-40 suggests the constraint has stabilized into a new equilibrium: legal frameworks are in place, institutional norms have shifted partially, and further gains require either deeper institutional change or face declining marginal returns. One shared time grid: every metric is authored at every sampled point (0, 5, 10, 15, 20, 25, 30, 40).
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (indigenous communities, knowledge keepers) experience this as coordination without extraction: oral tradition transmission is their epistemic right and cultural imperative, and research terms that respect it are not costs but prerequisites for legitimate knowledge work. The payer seats (credentialed institutions, external researchers) experience it as extraction: they lose unilateral authority, must fund research under constrained terms, and face reputational/legal consequences for non-compliance. From the external institutional seat, the constraint looks like tangled-rope becoming snare (coordination narrative eroding, extraction hardening). From the indigenous seat, it is pure coordination — the cost is enforcement against external frameworks that should never have had authority in the first place. The engine computes both seats' types from directionality: indigenous communities get low d (beneficiaries), pulling toward rope/coordination; credentialed institutions get high d (victims of epistemic subordination), pulling toward snare if suppression hardens further.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous communities are structural beneficiaries: they gain epistemic authority, control over interpretation, and repatriation of remains. Their directionality is near 0.0 (beneficiary end). Their time horizon is civilizational — the timeline of oral tradition transmission — and their exit is identity-locked: separating from oral tradition means cultural dissolution, so exit is incommensurable with identity. External researchers and credentialed institutions are structural victims: they lose interpretive authority, must negotiate under terms they did not set, and face legal and reputational costs. Their directionality is near 1.0 (target end). Their exit is constrained (researchers): abandoning the research means losing access to irreplaceable communities and archives; or mobile (institutions): they can redirect research elsewhere, but at reputational cost as the field normalizes community epistemology. Settler state institutions are analytical observers, but their position is active: they enforce the reading through law, so they function partly as agenda-setters. The two power atoms among payers (institutional) are differentiated by exit: researchers are trapped in specific communities; institutions can reallocate resources. No directionality override is needed; the derived d from beneficiary/victim + exit captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial denial of indigenous epistemology, looting of remains) is live: repatriation disputes, research-ethics battles, and institutional resistance continue. The disappearance verdict is world_rearranges: if this reading vanished, museums would reassert control, research would proceed under scientific method alone, and generations of knowledge transmission could be disrupted. The classification prevents misreading this as pure coordination (rope) by attending to suppression (0.71) and theater (0.41): as institutional resistance hardens, the constraint edges toward snare dynamics even though the reading's narrative remains coordination-focused. The mandate is not obsolete; it is contested. The classification as tangled_rope (not snare) holds because genuine coordination persists: oral tradition transmission is a real collective-action problem, and the reading's enforcement does solve it. But the suppression and theater metrics signal that the constraint is under stress — external institutions resist epistemic subordination, and increasing enforcement intensity is required to maintain the reading's authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_contest,
    'Is the anthropological record a kernel amenable to multiple readings (indigenous epistemology, scientific naturalism, creationist), or does one reading objectively capture what the record reveals?',
    'This is a conceptual question about the boundary between observable evidence and interpretive framework. Resolution would require philosophical clarification of what counts as ''the record'' independent of any reading — an unlikely terminus.',
    'If the record is genuinely kernel-like (admits multiple readings), this constraint models one legitimate reading among siblings; if one reading objectively matches the record, this reading is either vindicated or falsified by evidence. The kernel framing assumes the former.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_contest, conceptual, 'Whether the anthropological record is a kernel or a single-reading object').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of external research frameworks structural (external barriers: access denial, repatriation laws, institutional policy) or internalized (researchers have adopted oral tradition epistemology as their own framework)?',
    'Post-recognition behavioral patterns: if researchers treat oral tradition as binding after legal enforcement ends, suppression is partly internalized; if research continues as before once legal barriers lift, suppression is purely structural.',
    'If structural, the constraint persists because of external enforcement and would weaken if enforcement relaxes. If internalized, researchers have accepted the reading''s legitimacy and suppression persists even without legal force — a deeper constraint on credentialed science''s authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of external frameworks is structural or internalized').

omega_variable(
    knowledge_keeper_identity_lock_mechanism,
    'Is knowledge-keeper identity locked because oral tradition transmission IS their cultural identity (fused roles), or because exit costs are prohibitively high but identity remains separable?',
    'Counterfactual: if knowledge transmission stopped, would knowledge keepers still identify as such? If identity dissolves with transmission cessation, lock is identity-fusional; if identity persists as a historical role, lock is exit-cost-based.',
    'Pure identity fusion implies the reading is self-sustaining (no external enforcement needed); high exit cost but separable identity implies the reading depends on continued enforcement or cultural reproduction to hold. Classification-irrelevant to directionality computation, but important for understanding what would destabilize the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_keeper_identity_lock_mechanism, empirical, 'Whether knowledge-keeper identity lock is fusional or exit-cost-based').

omega_variable(
    sibling_reading_logical_structure,
    'Do the naturalist and creationist readings FORECLOSE this reading (make its core premise logically impossible within any framework), or do they merely COEXIST as held by different parties?',
    'Structural analysis of core axioms: if the naturalist axiom ''human origins are material/evolutionary'' and this reading''s axiom ''ancestral continuity is knowable via oral tradition'' can both be true (one describes mechanism, one describes epistemology), they coexist; if they directly contradict, foreclosure holds.',
    'Coexistence means the three readings are all live — the constraint models one reading in a contested triplet. Foreclosure would mean this reading succeeds only if naturalism fails — a different structural picture. The axiom-relation declarations assume coexistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_logical_structure, conceptual, 'Logical relationship between this reading and sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__indigenous_epistemology_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__indigenous_epistemology_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(anth_tr_t0, observed).
narrative_ontology:measurement(anth_tr_t5, anthropological_record__indigenous_epistemology_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(anth_tr_t5, observed).
narrative_ontology:measurement(anth_tr_t10, anthropological_record__indigenous_epistemology_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(anth_tr_t10, observed).
narrative_ontology:measurement(anth_tr_t15, anthropological_record__indigenous_epistemology_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(anth_tr_t15, observed).
narrative_ontology:measurement(anth_tr_t20, anthropological_record__indigenous_epistemology_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(anth_tr_t20, observed).
narrative_ontology:measurement(anth_tr_t25, anthropological_record__indigenous_epistemology_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(anth_tr_t25, observed).
narrative_ontology:measurement(anth_tr_t30, anthropological_record__indigenous_epistemology_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(anth_tr_t30, observed).
narrative_ontology:measurement(anth_tr_t40, anthropological_record__indigenous_epistemology_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(anth_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(anth_be_t0, observed).
narrative_ontology:measurement(anth_be_t5, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement_basis(anth_be_t5, observed).
narrative_ontology:measurement(anth_be_t10, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(anth_be_t10, observed).
narrative_ontology:measurement(anth_be_t15, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(anth_be_t15, observed).
narrative_ontology:measurement(anth_be_t20, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(anth_be_t20, observed).
narrative_ontology:measurement(anth_be_t25, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(anth_be_t25, observed).
narrative_ontology:measurement(anth_be_t30, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(anth_be_t30, observed).
narrative_ontology:measurement(anth_be_t40, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(anth_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(anth_su_t0, observed).
narrative_ontology:measurement(anth_su_t5, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(anth_su_t5, observed).
narrative_ontology:measurement(anth_su_t10, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(anth_su_t10, observed).
narrative_ontology:measurement(anth_su_t15, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(anth_su_t15, observed).
narrative_ontology:measurement(anth_su_t20, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(anth_su_t20, observed).
narrative_ontology:measurement(anth_su_t25, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(anth_su_t25, observed).
narrative_ontology:measurement(anth_su_t30, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(anth_su_t30, observed).
narrative_ontology:measurement(anth_su_t40, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(anth_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__indigenous_epistemology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(anthropological_record__indigenous_epistemology_reading, 0.12).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__creationist_reading).

% DUAL FORMULATION NOTE:
% The anthropological record is a contested kernel admitting three structurally distinct readings grounded in different epistemologies. This story instantiates the indigenous-epistemology reading (oral tradition as primary source, community authority over interpretation). Sibling readings (naturalist, creationist) are authored as separate constraint stories with their own ε, beneficiary/victim structures, and enforcement dynamics, linked via network.affects_constraints. The kernel family admits these three readings simultaneously across different parties (indigenous communities hold this reading; academic institutions hold naturalist; creationist communities hold creationist); no single party holds all three. Classification diverges across readings: this reading is tangled-rope (coordination + asymmetric extraction); naturalist reading is rope or scaffold (pure coordination, or temporary coordination pending scientific settlement); creationist reading is snare (pure extraction riding theological framing). The family structure models the contest as structural constraint diversity, not as metric ambiguity within a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
