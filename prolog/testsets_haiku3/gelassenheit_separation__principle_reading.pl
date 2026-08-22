% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__principle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__principle_reading, []).

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
 *   constraint_id: gelassenheit_separation__principle_reading
 *   human_readable: Gelassenheit Separation Principle: Functional Isolation Doctrine
 *   domain: religious/commitment_systems
 *
 * SUMMARY:
 *   The principle reading of Gelassenheit separation doctrine holds that
 *   technology is acceptable if structurally isolated from worldly system
 *   entanglement. This reading permits off-grid solar panels and pneumatic
 *   tools while forbidding Internet and insurance regardless of functional
 *   isolation possibilities. It is one reading of a contested kernel shared
 *   with an artifact reading (visible distinction) and a consequence reading
 *   (behavioral preservation). The principle reading emerged in the mid-20th
 *   century as a philosophical reinterpretation of earlier Anabaptist
 *   theology, offering a more conceptually sophisticated rationale for
 *   technology restrictions that had originally been justified through
 *   visible markers of separation. This story generates the principle reading
 *   as a clean constraint with its own epsilon, beneficiary/victim structure,
 *   and suppression mechanisms, distinct from its siblings. The kernel
 *   contest is routed through omega variables and the cs_structure block per
 *   the committer frame rules.
 *
 * KEY AGENTS:
 *   - Anabaptist community leadership: institutional power, identity-locked authority over doctrine interpretation
 *   - Individual technology adopters: powerless, identity-locked into the community, bear the costs of boundary enforcement
 *   - Boundary-case families: powerless, constrained exit, navigate enforcement inconsistency
 *   - Traditional authority preservers: organized power, identity-locked, benefit from a framework that preserves collective infrastructure
 *   - Theological interlocutors: moderate power, mobile exit, shape the interpretive frame through scholarly work
 *   - Artifact and consequence reading communities: excluded, would object if heard, trapped within regional Anabaptist discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__principle_reading, 0.48).
domain_priors:suppression_score(gelassenheit_separation__principle_reading, 0.52).
domain_priors:theater_ratio(gelassenheit_separation__principle_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__principle_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__principle_reading, "Gelassenheit Separation Principle: Functional Isolation Doctrine").
narrative_ontology:topic_domain(gelassenheit_separation__principle_reading, "religious/commitment_systems").

domain_priors:requires_active_enforcement(gelassenheit_separation__principle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__principle_reading, '3b4ecc33-9e38-4856-a872-bc11912afb99').
narrative_ontology:cs_kernel_codification('3b4ecc33-9e38-4856-a872-bc11912afb99', distributed).
narrative_ontology:cs_authority_grounding('3b4ecc33-9e38-4856-a872-bc11912afb99', lineage).
narrative_ontology:cs_interpretation_layer_present('3b4ecc33-9e38-4856-a872-bc11912afb99').
narrative_ontology:cs_reading_relation('3b4ecc33-9e38-4856-a872-bc11912afb99', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_reading_relation('3b4ecc33-9e38-4856-a872-bc11912afb99', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('3b4ecc33-9e38-4856-a872-bc11912afb99', foundational, structural_entanglement_primacy).
narrative_ontology:cs_axiom_status(structural_entanglement_primacy, holdable).
narrative_ontology:cs_axiom_grounding('3b4ecc33-9e38-4856-a872-bc11912afb99', structural_entanglement_primacy, deontological).
narrative_ontology:cs_axiom('3b4ecc33-9e38-4856-a872-bc11912afb99', foundational, functional_isolation_sufficiency).
narrative_ontology:cs_axiom_status(functional_isolation_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('3b4ecc33-9e38-4856-a872-bc11912afb99', functional_isolation_sufficiency, instrumental).
narrative_ontology:cs_reference_frame('3b4ecc33-9e38-4856-a872-bc11912afb99', autonomous_technology_by_structural_isolation).
narrative_ontology:cs_drift_state('3b4ecc33-9e38-4856-a872-bc11912afb99', contemporary_internet_ubiquity, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3b4ecc33-9e38-4856-a872-bc11912afb99', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(gelassenheit_separation__principle_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, anabaptist_community_leadership).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, traditional_authority_preservers).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, individual_technology_adopters).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, boundary_case_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, individual_technology_adopters).
narrative_ontology:constraint_vindicates(gelassenheit_separation__principle_reading, worldly_system_autonomy_doctrine).
narrative_ontology:constraint_vindicates(gelassenheit_separation__principle_reading, structural_separation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the separation principle within the Anabaptist community. They adjudicate which technologies are functionally isolated and which entangle with worldly systems. Their authority rests on genealogical continuity with the founding community's practices and their reading of Gelassenheit doctrine. They enforce through social pressure, ritual participation gating, and membership standing.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, anabaptist_community_leadership, agenda_setter,
    organized, generational, identity_locked, regional).

% Wish to adopt specific technologies (solar panels, pneumatic tools, Internet access for business or education) while remaining in the community. They argue that functional isolation from worldly systems is what matters — a solar panel off-grid is categorically different from a smartphone networking to distant markets. They bear the cost of exclusion, shunning, or forced adoption/rejection that contradicts their framing. Their exit from the community means loss of kin networks, shared land, mutual aid arrangements, and identity.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, individual_technology_adopters, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__principle_reading, individual_technology_adopters, beneficiary).

% Live in ambiguous technological zones where the principle-vs-artifact tension is acute: satellite Internet for weather forecasting on farmland, insurance purchased through a collective broker, electrical systems powered by renewable microgrids. They navigate enforcement uncertainty because the leadership's rulings apply the principle inconsistently across domains, creating situations where the same functional logic (off-grid) is permitted for solar but forbidden for Internet.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, boundary_case_families, payer,
    powerless, biographical, constrained, local).

% Benefit from a framework that keeps the community visibly distinct from English society and technologically dependent on shared infrastructure (collective barns, shared equipment pools, face-to-face coordination). They interpret the principle as a guardrail against individual autonomy and technological atomization. A principle-based doctrine gives them a rationale that sounds deontological rather than purely conservative.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, traditional_authority_preservers, beneficiary,
    organized, generational, identity_locked, regional).

% Study and write about Gelassenheit, Anabaptist theology, and technology ethics from outside the lived community. They produce the interpretive frame through which the principle is debated: whether 'separation' is primarily an ontological stance (disengagement from worldly system logic), a visible marker (distinction), or a behavioral outcome (preserved practices). Their theological work shapes which readings are credible.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, theological_interlocutors, observer,
    moderate, generational, mobile, regional).

% Members and historians who hold the artifact reading — that separation is primarily visible distinction, and technology is forbidden if it resembles English worldly artifacts. They are excluded from this story's adjudication; their reading creates structural pressure that contradicts the principle reading's functional logic. If present in the decision forum, they would argue that a solar panel's visual appearance and its place in the world of consumer products makes it problematic, regardless of functional isolation.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, artifact_reading_community, excluded,
    organized, generational, trapped, regional).

% Members and theologians who hold the consequence reading — that separation means preserving community practices of visiting, mutual aid, and geographic rootedness. They are excluded from this principle-reading's frame. Their reading emphasizes that whether a technology entangles depends on its impact on face-to-face coordination and shared lifeways, not on whether it is functionally isolated. A solar panel that enables a family to operate independently of the collective barn would fail the consequence-reading test even if functionally isolated.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, consequence_reading_community, excluded,
    organized, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__principle_reading, anabaptist_community_leadership).
narrative_ontology:fixing_cost_class(gelassenheit_separation__principle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the community's understanding of itself as set-apart from the logic of worldly system integration. The principle reading provides a hermeneutic framework for collective discernment about technology adoption: decisions can be argued on the basis of structural entanglement rather than aesthetic distinction or behavioral upheaval alone. It coordinates internal theological reasoning and external boundary-maintenance work.
% TRANSFER_FUNCTION: Moves the authority to interpret what constitutes 'functional isolation' from individual conscience and household judgment to the organized leadership structure. Community members who believe they have adopted a functionally isolated technology must submit their judgment to collective discernment and accept the leadership's framing. The cost of technology adoption is transmitted to whoever deviates from the collective's ruling — loss of standing, social exclusion, or forced choice between the technology and community membership.
% ABSENT_VOICES: Members who have adopted the artifact reading (technology is about visual distinction) and the consequence reading (technology is about behavioral impact) are structurally excluded from deciding what the principle means. Their exclusion is enforced by the leadership's authority to define the kernel. They would argue that the principle reading is a rationalization that permits technological autonomy while preserving appearance of separation. Historians and theologians from outside the community might argue that the principle reading is a modern philosophical interpretation, not a traditional understanding of Gelassenheit.
% DISAPPEARANCE_RATIONALE: If the principle-based separation doctrine vanished, the community would either revert to artifact-based or consequence-based framings (existing alongside in some regions), or fragment into splinter groups with different technology policies. Individual household autonomy over technology would increase; collective infrastructure sharing would decrease. The leadership's rationale for enforcing technology restrictions would weaken from 'structural entanglement' to purely traditional/aesthetic grounds, which would face higher resistance.
% FOUNDING_PROBLEM: Early Anabaptist communities (16th-17th centuries) needed to remain visibly distinct from surrounding society to preserve their identity and survive persecution. Technology was understood through the lens of visible separation. As communities became more stable and affluent, technology adoption (electricity, engines, industrial goods) created pressure to update the separation doctrine from artifact-based (forbid machines that look worldly) to principle-based (permit machines that are functionally isolated). The principle reading emerged as a compromise that sounded modern and philosophical while preserving conservative outcomes.
% FOUNDING_PROBLEM_CORROBORATION: Anabaptist historians and theologians outside the community (not beneficiaries of the doctrine) attest that the principle reading is a 20th-century reinterpretation, not a founding doctrine. They point to historical texts where Gelassenheit was understood as visible separation and behavioral continuity, not functional isolation. Community leaders attest that the principle reading is a faithful development of founding theology. Individual technology adopters attest that the principle reading is being applied inconsistently to preserve conservative outcomes, suggesting the founding problem (maintaining separation) has shifted into justifying authority.
narrative_ontology:disappearance_verdict(gelassenheit_separation__principle_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__principle_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__principle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gelassenheit_separation__principle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__principle_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__principle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gelassenheit_separation__principle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gelassenheit_separation__principle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because the principle reading genuinely does permit some technologies that an artifact reading would forbid, creating a real coordination function around functional isolation. However, extraction persists because the leadership retains the authority to define what 'functionally isolated' means, and this authority is applied inconsistently — solar is permitted when communal benefit is demonstrated, Internet is forbidden even for off-grid use, insurance is forbidden despite collective-broker isolation. The measurement trajectory shows extractiveness rising through t=25 (0.38→0.48) as more technology pressures emerge, then stabilizing as the leadership's rulings calcify and people internalize the doctrine. Suppression rises more steeply (0.38→0.52) because enforcement requires not just ruling on cases but maintaining the principle's philosophical coherence against empirical pressure: solar panels function off-grid, Internet can function off-grid, yet the doctrine forbids one and permits the other. Theater rises slowly (0.12→0.22) as the leadership spends more effort justifying the principle's consistency to skeptics within and outside the community. At t=0, the principle reading is a live philosophical option with real coordination potential. By t=50, it has stabilized into a framework that preserves leadership authority while permitting only narrow technological concessions.
 *
 * PERSPECTIVAL GAP:
 *   The principle reading is claimed by both the leadership and individual adopters as the 'true' doctrine, but they derive opposite conclusions from it. This is the core perspectival asymmetry: a principle sounds objective and rule-like until enforcement requires interpretation, at which point authority becomes the operative force. The leadership's interpretation privilege is structurally protected by their organized power, their identity-lock to the community, and their role as theological custodians. The adopters' arguments that 'off-grid means off-grid' are logically sound but lack institutional standing to override the leadership's judgment.
 *
 * DIRECTIONALITY LOGIC:
 *   The principle reading permits technologies that are functionally isolated from worldly system entanglement. A solar panel off-grid is not connected to energy markets, supply chains, or the logic of consumer choice — it is accepted. Internet, even off-grid through a local mesh network, is forbidden because it connects to the logic of global information markets and distant institutions. Insurance is forbidden even if purchased collectively because it represents a risk-pooling arrangement with the commercial market. This logic is internally coherent. However, the leadership's application of it is selective: when collective solar would threaten the barn infrastructure (shared equipment, face-to-face coordination), leadership sometimes rules it unnecessary or risky; when household solar threatens nothing, it is permitted. This selective application is where extraction emerges: the leadership extracts the authority to define isolation, not just to communicate the principle.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the principle reading as pure coordination (rope) by forcing explicit attention to the extraction mechanism: the leadership's sole authority to define 'functional isolation,' applied inconsistently. It also prevents mislabeling it as pure extraction (snare) by acknowledging the real coordination function that the doctrine does serve — it gives individuals a framework for thinking about technology in terms of structural entanglement rather than merely conforming to authority. The classification as tangled rope is appropriate: genuine coordination (articulate a principle for technology discernment) and genuine extraction (concentrate authority over interpretation) are wired together. Unraveling them would require either a clear computational rule (off-grid means permitted, period) or a full transfer of judgment authority to households, either of which would dissolve the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    principle_vs_artifact_framing,
    'Is separation fundamentally a principle about structural non-entanglement (allowing solar panels, forbidding Internet), or is it fundamentally about visible distinction (forbidding anything that resembles commercial technology)? Can one commitment framework hold both readings, or do they foreclose each other?',
    'Historical and theological analysis of founding texts and early Anabaptist practice; comparative ethnographic study of different Anabaptist communities that have adopted different readings; observation of which reading members invoke when defending decisions to other communities.',
    'If the readings foreclose each other, the kernel admission of multiple readings is unstable — one will eventually dominate. If they coexist, the constraint''s extractiveness may be driven by leadership''s choice of which reading to emphasize in different contexts, making the choice itself the extraction mechanism. If they can be unified, the apparent inconsistency in the principle reading''s application (solar yes, Internet no) reflects an unacknowledged artifact component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(principle_vs_artifact_framing, conceptual, 'Whether principle and artifact readings are coherent alternatives or mutually foreclosing interpretations.').

omega_variable(
    functional_isolation_verifiability,
    'Can ''functional isolation'' be defined clearly enough to be a rule, or does it require ongoing interpretation authority?',
    'Attempt to draft a computational decision procedure for functional isolation (off-grid solar yes, Internet no, insurance no, etc.) and determine whether the procedure matches the leadership''s actual rulings or whether the rulings deviate from it; determine whether members can predict future rulings.',
    'If functional isolation can be clearly defined, the constraint could transition to a rope (pure coordination with a bright-line rule). If it requires ongoing interpretation, the extractiveness is higher because authority is irreducible — the constraint is structurally tangled rather than coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_isolation_verifiability, empirical, 'Whether the principle reading is operationalizable as a rule or requires permanent interpretive authority.').

omega_variable(
    identity_lock_mechanism,
    'Is the suppression of resistance to the principle reading structural (exit is genuinely costly, kin networks are geographically tied, alternative lives are unavailable) or internalized (individuals have fused their identity with the community such that questioning the doctrine feels like betrayal)?',
    'Longitudinal tracking of individuals who leave the community: do they report that the cost structure held them in place initially, then they adapted their identity after exit, or do they report having previously believed the doctrine was true until reflection after leaving? Do they maintain community contact if permitted to adopt the forbidden technology?',
    'If suppression is structural, the constraint''s effective suppression is accurately measured as 0.52 and would decline if exit options expanded. If suppression is internalized, the constraint carries the suppression with ex-members — it persists even after formal exit. The distinction affects whether temporal liberalization (more technology options made available) would reduce the constraint''s grip on individual decision-making.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether the constraint''s suppression is structural or internalized via identity fusion.').

omega_variable(
    kernel_reading_contest_structure,
    'Does the principle reading represent a genuine alternative framing of Gelassenheit, or is it a rationalization that masks the persistence of the artifact reading under new terminology?',
    'Close reading of how the leadership applies the principle to edge cases: when they forbid Internet, do they use principle language (structural entanglement) or revert to artifact language (it looks worldly, it brings the English world into our homes)? Do they apply the principle consistently, or do they switch to artifact reasoning when principle reasoning would require permitting something they want to forbid?',
    'If the principle reading is a genuine alternative, it is a separate constraint from the artifact reading, even though they may produce overlapping outcomes. If it is a rationalization, the constraint''s real mechanism is artifact-based (visible separation) disguised in principle language, which would change the classification — the theater ratio is higher than authored, and the coordination function is actually quite different from what the principle reading claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Whether the principle reading is a distinct hermeneutical option or a cover story for artifact-based separation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__principle_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__principle_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(gela_tr_t0, observed).
narrative_ontology:measurement(gela_tr_t8, gelassenheit_separation__principle_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement_basis(gela_tr_t8, observed).
narrative_ontology:measurement(gela_tr_t16, gelassenheit_separation__principle_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement_basis(gela_tr_t16, observed).
narrative_ontology:measurement(gela_tr_t25, gelassenheit_separation__principle_reading, theater_ratio, 25, 0.21).
narrative_ontology:measurement_basis(gela_tr_t25, observed).
narrative_ontology:measurement(gela_tr_t37, gelassenheit_separation__principle_reading, theater_ratio, 37, 0.22).
narrative_ontology:measurement_basis(gela_tr_t37, observed).
narrative_ontology:measurement(gela_tr_t50, gelassenheit_separation__principle_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement_basis(gela_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__principle_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(gela_be_t0, observed).
narrative_ontology:measurement(gela_be_t8, gelassenheit_separation__principle_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement_basis(gela_be_t8, observed).
narrative_ontology:measurement(gela_be_t16, gelassenheit_separation__principle_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement_basis(gela_be_t16, observed).
narrative_ontology:measurement(gela_be_t25, gelassenheit_separation__principle_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement_basis(gela_be_t25, observed).
narrative_ontology:measurement(gela_be_t37, gelassenheit_separation__principle_reading, base_extractiveness, 37, 0.48).
narrative_ontology:measurement_basis(gela_be_t37, observed).
narrative_ontology:measurement(gela_be_t50, gelassenheit_separation__principle_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement_basis(gela_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__principle_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(gela_su_t0, observed).
narrative_ontology:measurement(gela_su_t8, gelassenheit_separation__principle_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement_basis(gela_su_t8, observed).
narrative_ontology:measurement(gela_su_t16, gelassenheit_separation__principle_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement_basis(gela_su_t16, observed).
narrative_ontology:measurement(gela_su_t25, gelassenheit_separation__principle_reading, suppression_requirement, 25, 0.51).
narrative_ontology:measurement_basis(gela_su_t25, observed).
narrative_ontology:measurement(gela_su_t37, gelassenheit_separation__principle_reading, suppression_requirement, 37, 0.52).
narrative_ontology:measurement_basis(gela_su_t37, observed).
narrative_ontology:measurement(gela_su_t50, gelassenheit_separation__principle_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement_basis(gela_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__principle_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__principle_reading, 0.1).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% The gelassenheit_separation kernel is contested across three readings, each producing a distinct constraint with different epsilon values, beneficiary/victim structures, and suppression mechanisms. The principle_reading (this story) permits technologies that are functionally isolated from worldly system entanglement, creating moderate extractiveness (0.48) through the leadership's authority to interpret what 'functional isolation' means. The artifact_reading produces higher suppression (visible conformity is easier to enforce) and potentially lower extractiveness (the rule is bright-line). The consequence_reading focuses on behavioral impact (visiting, mutual aid, geographic rootedness) and would target different victims and beneficiaries. These are three separate constraints linked through the kernel they share, not three perspectives on a single constraint. The decomposition follows OQ-26 ε-invariance: each reading has a distinct ε referent (what constitutes violation: structural entanglement vs. visible resemblance vs. behavioral disruption) and produces different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gelassenheit_separation__principle_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
