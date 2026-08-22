% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__artifact_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__artifact_reading, []).

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
 *   constraint_id: gelassenheit_separation__artifact_reading
 *   human_readable: Gelassenheit Separation by Artifact Appearance (Mennonite/Amish Reading)
 *   domain: religious/cultural/technological
 *
 * SUMMARY:
 *   This constraint story instantiates the artifact_reading of the
 *   Gelassenheit separation kernel: separation is visible distinction from
 *   English society achieved by forbidding technologies that resemble worldly
 *   artifacts, regardless of their actual function or effect on community
 *   practice. A solar panel is forbidden because it looks modern, not because
 *   it undermines self-sufficiency or dependence on grid systems. A synthetic
 *   work shirt is forbidden because it resembles English-world wear, not
 *   because it damages community bonds or visiting patterns. This reading
 *   prioritizes the visual marker over the functional or relational logic.
 *   The artifact standard is enforced by institutional authority (bishops and
 *   elders) through shunning and church discipline. The constraint shows high
 *   extractiveness (0.82): it forbids technologies that members themselves
 *   recognize as functionally beneficial, suppressing their own pragmatic
 *   reasoning. Theater is substantial (0.62): much of enforcement effort goes
 *   to maintaining the appearance boundary rather than addressing the
 *   underlying coordination problem (avoiding assimilation), and the rhetoric
 *   of Gelassenheit (yielding to God's will) masks the institutional power
 *   maintaining the aesthetic standard.
 *
 * KEY AGENTS:
 *   - community_authority_holders: institutional power, maintains the artifact interpretation and enforces visible distinction via discipline
 *   - pragmatic_farmers: moderate power, want solar panels and efficient fabrics; identity-locked to community; bear the extraction
 *   - younger_community_members: powerless, face internalized suppression of functional reasoning about technologies
 *   - technology_adopters: moderate power, face discipline and shunning for violating the artifact standard
 *   - consequence_reading_holders: minority within community, excluded from decision authority, must comply with artifact standard
 *   - principle_reading_holders: minority within community, structurally suppressed by artifact-standard enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, 0.82).
domain_priors:suppression_score(gelassenheit_separation__artifact_reading, 0.89).
domain_priors:theater_ratio(gelassenheit_separation__artifact_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, suppression_requirement, 0.89).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__artifact_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__artifact_reading, "Gelassenheit Separation by Artifact Appearance (Mennonite/Amish Reading)").
narrative_ontology:topic_domain(gelassenheit_separation__artifact_reading, "religious/cultural/technological").

domain_priors:requires_active_enforcement(gelassenheit_separation__artifact_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__artifact_reading, '6e592a5d-5e07-440b-bf97-2072fb53ea78').
narrative_ontology:cs_kernel_codification('6e592a5d-5e07-440b-bf97-2072fb53ea78', distributed).
narrative_ontology:cs_authority_grounding('6e592a5d-5e07-440b-bf97-2072fb53ea78', extraction).
narrative_ontology:cs_interpretation_layer_present('6e592a5d-5e07-440b-bf97-2072fb53ea78').
narrative_ontology:cs_reading_relation('6e592a5d-5e07-440b-bf97-2072fb53ea78', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e592a5d-5e07-440b-bf97-2072fb53ea78', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_axiom('6e592a5d-5e07-440b-bf97-2072fb53ea78', foundational, artifact_appearance_determines_worldliness).
narrative_ontology:cs_axiom_status(artifact_appearance_determines_worldliness, holdable).
narrative_ontology:cs_axiom_grounding('6e592a5d-5e07-440b-bf97-2072fb53ea78', artifact_appearance_determines_worldliness, conventional).
narrative_ontology:cs_axiom('6e592a5d-5e07-440b-bf97-2072fb53ea78', foundational, visible_distinction_constitutes_separation).
narrative_ontology:cs_axiom_status(visible_distinction_constitutes_separation, holdable).
narrative_ontology:cs_axiom_grounding('6e592a5d-5e07-440b-bf97-2072fb53ea78', visible_distinction_constitutes_separation, deontological).
narrative_ontology:cs_reference_frame('6e592a5d-5e07-440b-bf97-2072fb53ea78', early_anabaptist_defensive_separation).
narrative_ontology:cs_drift_state('6e592a5d-5e07-440b-bf97-2072fb53ea78', contemporary_pluralistic_context, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6e592a5d-5e07-440b-bf97-2072fb53ea78', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(gelassenheit_separation__artifact_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, community_authority_holders).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, visible_separation_doctrine).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, technology_adopters).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, pragmatic_farmers).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, younger_community_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, religious_tradition_interpreters).
narrative_ontology:constraint_vindicates(gelassenheit_separation__artifact_reading, artifact_appearance_determines_worldliness).
narrative_ontology:constraint_vindicates(gelassenheit_separation__artifact_reading, visible_distinction_from_english_society).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops, elders, and tradition-keepers who interpret and enforce the artifact standard. They maintain the boundary between acceptable (plain dress, horse transport, hand tools) and forbidden (synthetic fabrics that resemble worldly wear, electric equipment regardless of function, solar panels that look modern). They administer church discipline including shunning for violations. Their authority rests on maintaining visible distinction as the primary marker of separate community identity.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, community_authority_holders, agenda_setter,
    institutional, generational, trapped, local).

% Want to adopt solar panels for off-grid power (functionally identical to a hidden power source but forbidden because the equipment looks modern), or efficient synthetic fabrics for work clothes (functionally superior but forbidden because they resemble English-world wear). They understand the functional logic—a solar panel powers a house; a synthetic shirt is just cloth—but must either suppress this understanding or face discipline. Their exit means leaving community, family, and land.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, pragmatic_farmers, payer,
    moderate, biographical, identity_locked, local).

% Grow up learning the artifact standard as natural law but encounter the functional argument in their own reasoning and from peers. The constraint forbids them from adopting technologies that would ease their work or improve their families' material circumstances—not because the technology itself is harmful, but because it looks wrong. They must choose between pragmatic adoption (with discipline consequences) or internalized suppression of the rational case for the technology.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, younger_community_members, payer,
    powerless, biographical, identity_locked, local).

% Members who have adopted forbidden technologies (solar panels, synthetic work fabrics, efficient tools that resemble worldly equipment) and face escalating church discipline: first informal shunning, then formal Meidung (shunning) if they refuse to put the equipment away. The constraint costs them the technology, income from selling the technology, and in cases of full shunning, family relationships and economic participation.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, technology_adopters, payer,
    moderate, biographical, constrained, local).

% Would argue the constraint is irrational (forbidding functional technologies on appearance alone) but have no standing in community discipline. They observe the constraint from outside and are kept outside by the same visible-distinction logic—their electric tools and synthetic clothes are marks of their outsider status. They are the contrast class the constraint maintains.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, english_english_speaking_neighbors, excluded,
    moderate, biographical, analytical, local).

% Theologians and historians who maintain the artifact-reading interpretation of Gelassenheit (yielding to God's will expressed through visible separation) in scholarly and ecclesiastical discourse. They benefit from the constraint's persistence in keeping the artifact standard as the canonical reading. Their professional authority and publication output depend partly on maintaining this interpretation against competing readings.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, religious_tradition_interpreters, beneficiary,
    institutional, generational, analytical, regional).

% Minority within the community who hold the consequence_reading (separation means preserving practices like visiting and mutual aid; technologies acceptable if they don't harm community bonds). They argue that solar panels preserve self-sufficiency and family time (supporting visiting practices), but are overruled by the dominant artifact-reading authority. They must comply or face discipline.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, consequence_reading_holders, excluded,
    moderate, biographical, constrained, local).

% Smaller minority holding the principle_reading (separation means structural isolation from worldly economic systems; technologies acceptable if functionally self-contained). They argue a solar panel is structurally isolated from grid dependence and thus consistent with Gelassenheit, but find no hearing in the dominant artifact-reading authority structure. Their suppression is structural—the artifact standard is enforced discipline.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, principle_reading_holders, excluded,
    powerless, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__artifact_reading, community_authority_holders).
narrative_ontology:fixing_cost_class(gelassenheit_separation__artifact_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains visible, collective boundary markers that distinguish the community from surrounding English society—plain dress, absence of electric equipment, hand tools, horse transport. The shared aesthetic and material practice is the coordination problem solved: how does a community maintain a coherent identity and mutual recognition when surrounded by a dominant culture? The artifact standard coordinates this through enforced visual consistency.
% TRANSFER_FUNCTION: Moves autonomy and technological choice from individual actors (pragmatic farmers, younger members, technology-adopters) to the collective authority (elders, bishops). Community members bear the cost of foregone technologies (efficiency losses, economic disadvantage, internal cognitive dissonance). The authority and the vindicated artifact-doctrine proposition collect the benefit: maintained institutional control and doctrinal preservation.
% ABSENT_VOICES: Members who left the community over technological restrictions (ex-members); competing theological interpreters (consequence_reading and principle_reading holders) who are present but structurally excluded from authority; English-speaking neighbors whose pragmatic objections are kept outside the decision frame by the same boundary the constraint maintains.
% DISAPPEARANCE_RATIONALE: If the artifact restriction vanished overnight, community members would immediately adopt solar panels, modern fabrics, and efficient equipment; they would maintain the same communal land and visiting patterns but with reduced material deprivation. The visible distinction would blur at the margins. The community would continue—the functional work of maintaining bonds remains—but the aesthetic boundary would soften unless authority chose to maintain it through other means.
% FOUNDING_PROBLEM: Early Anabaptist/Mennonite communities faced pressure to assimilate into dominant Protestant and secular societies. Visible difference (plain dress, refusal of military service, separation of community from state structure) marked identity and reduced intermarriage and cultural drift. The artifact standard emerged as a way to make separation visible and unmistakable at all times.
% FOUNDING_PROBLEM_CORROBORATION: Community authorities attest the problem is live and ongoing—assimilation pressure continues and visible distinction remains necessary. Historians and theological interpreters who hold the artifact_reading support this. However, consequence_reading and principle_reading holders within the community, plus external scholars of Anabaptism, attest that the founding problem (assimilation pressure on core practices) is at least substantially reduced in modern pluralistic societies where minorities are legally protected and culturally tolerated. They argue the visible-artifact focus has become the constraint's own justification rather than the solving of the founding problem.
narrative_ontology:disappearance_verdict(gelassenheit_separation__artifact_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__artifact_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__artifact_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gelassenheit_separation__artifact_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__artifact_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__artifact_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gelassenheit_separation__artifact_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at end) because the constraint forbids technologies on appearance alone, overriding pragmatic reasoning and functional benefit. Suppression is highest (0.89) because members internalize the artifact standard as natural/divine law despite encountering the functional argument repeatedly. Theater is substantial (0.62) because enforcement effort goes increasingly to maintaining the aesthetic boundary (inspecting equipment, enforcing dress codes) rather than addressing the real coordination problem (preventing assimilation through cultural drift). The measurement series tracks the increasing burden of suppression as external pressure (English technology ubiquity, younger members' exposure to functional arguments) rises but the constraint tightens enforcement to maintain the boundary. Extraction accumulates because authority must invest more suppressive capacity to keep the same boundary as the cultural environment shifts.
 *
 * PERSPECTIVAL GAP:
 *   From the authority seat: the artifact standard is the visible expression of Gelassenheit and community identity; the constraint coordinates necessary distinction. From the payer seat: the artifact standard forbids technologies that would improve their lives without harming the community's actual functions (visiting, mutual aid, geographic rootedness). The engine computes these seats differently from the structural data: the authority seat derives low directionality (benefits from constraint persistence, high power, exit = institutional death), while the payer seats derive high directionality (suppressed, constrained exit, benefits withheld). This gap is the measurement the constraint story captures.
 *
 * DIRECTIONALITY LOGIC:
 *   Community authority holders are the structural beneficiaries: they collect institutional power, doctrinal authority, and the ability to define community membership through enforcement of the artifact standard. Directionality for authority ~0.1 (near beneficiary end). Pragmatic farmers, technology adopters, and younger members are structural targets: they are forbidden technologies on appearance alone; their functional reasoning is suppressed; their exit is identity-locked (leaving means losing family, land, community). Directionality for payers ~0.85-0.95 (near target end). The vindicated proposition (artifact-appearance-determines-worldliness) is NOT an agent and receives no benefits; it is listed to distinguish what the constraint vindicates from what actual agents collect.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—assimilation pressure from surrounding dominant society—was live when the artifact standard emerged but is arguably substantially diminished in modern pluralistic societies where religious minorities have legal protections and cultural tolerance. However, the artifact standard persists and even tightens (higher suppression_requirement over time) despite the founding problem's reduced pressure. This is mandatrophy: the constraint's primary justification (preventing assimilation) has become less urgent, but institutional authority maintains the constraint for its own stability and the vindication of the artifact doctrine. The theater ratio rising (0.48 to 0.62) reflects this: enforcement effort increasingly goes to maintaining the aesthetic boundary itself rather than solving the assimilation problem. The classification (Tangled Rope) captures this: there IS genuine coordination (community identity maintenance), but it is increasingly wedded to asymmetric extraction (authority + doctrine benefit from the artifact standard; payers bear the cost of foregone technology). Without the extraction, the constraint would be a Rope (pure coordination on visible distinction). With it, the extraction increasingly dominates the function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    artifact_vs_function_kernel_ambiguity,
    'Does Gelassenheit (yielding to God''s will) mandate visible distinction from worldly appearance, or does it mandate avoiding structural entanglement with worldly systems? The artifact_reading chooses the first; the principle_reading chooses the second.',
    'Historical-textual analysis: what did early Anabaptist writers (Menno Simons, Dutch Brethren) emphasize as the reason for separation? Lived-experience data: do communities with consequence_reading or principle_reading implementations (allowing solar panels, synthetic fabrics) experience greater assimilation or loss of community bonds? Comparative data across Mennonite and Amish subgroups.',
    'If the principle_reading is correct (structure matters, appearance is secondary), the artifact_reading is a false summit: it forbids technologies on appearance while claiming to serve Gelassenheit, but actually uses appearance as a proxy for control. If the artifact_reading is correct (visible distinction IS the means of separation), then technologies must be forbidden on appearance alone regardless of function. The classification shifts from high-extraction Tangled Rope (artifact_reading) to moderate-extraction Rope (principle_reading) if the kernel is resolved toward structure over appearance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artifact_vs_function_kernel_ambiguity, conceptual, 'The fundamental kernel question: does Gelassenheit mandate visible appearance or structural isolation?').

omega_variable(
    internalized_suppression_mechanism,
    'Is the measured suppression (0.89) structural (external discipline, shunning, economic costs) or internalized (members believe the artifact standard is correct even when they recognize the functional argument against it)?',
    'Post-exit survey: members who left the community over technology restrictions—do they immediately adopt the forbidden technologies or do they maintain the artifact standard from internalized belief? Interviews with pragmatic_farmers and younger members about their internal reasoning (do they think the standard is correct, or that they must obey despite thinking it wrong?). Trajectory of ex-members'' technology adoption patterns.',
    'If suppression is primarily internalized, the constraint''s effective power persists even after formal enforcement mechanisms are removed (members carry the suppression with them). If structural, members would quickly adopt forbidden technologies after leaving discipline. Internalized suppression raises the effective extraction (cost to the payer includes the suppressed reasoning capacity). Mixed suppression (structural + internalized) suggests identity-fusion (members fuse their identity with the artifact standard).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_mechanism, empirical, 'Whether suppression is structural or internalized in the payer seats.').

omega_variable(
    authority_capture_of_doctrine,
    'Does the artifact_reading reflect a genuine theological consensus, or has institutional authority (bishops, elders) captured the interpretation of the Gelassenheit kernel to preserve their own power?',
    'Historical lineage analysis: when did the artifact_reading become dominant relative to the consequence_reading and principle_reading? Did the shift correlate with institutional centralization or pressure from assimilation? Demographic data: do communities with more distributed authority (less bishop power) show higher acceptance of consequence_reading or principle_reading alternatives? Contention analysis: how much active resistance exists within the community to the artifact interpretation?',
    'If authority has captured the interpretation (false summit pattern), the constraint''s classification should flag institutional capture—the apparent coordination (maintaining community identity) is cover for extraction (maintaining institutional control). The vindicated proposition would be falsely summited. If the artifact_reading is genuinely held across the tradition, the classification stands as Tangled Rope with genuine coordination underlying the extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_capture_of_doctrine, empirical, 'Whether the artifact_reading reflects consensus or institutional capture.').

omega_variable(
    reading_coexistence_stability,
    'Can the artifact_reading and the consequence_reading / principle_reading coexist stably within a single community framework, or does the artifact_reading''s enforcement necessarily suppress the alternatives?',
    'Observational data from communities where multiple readings are tolerated (e.g., some Mennonite groups allow solar panels officially while others enforce artifact standards). Do these coexist without active conflict, or does one reading gradually dominate? Comparison with other religious traditions (Catholic-Orthodox schisms, Sunni-Shia fiqh schools) where readings coexist with formal boundaries—does the structure parallel this constraint''s structure?',
    'If readings can coexist, the constraint is better understood as one enforced reading suppressing alternatives rather than the sole coherent interpretation of the kernel. If one reading necessarily dominates, the constraint''s enforcement is inevitable within any institutional structure. Stability of coexistence affects whether reform is achievable through dialogue vs. requiring institutional change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_stability, empirical, 'Whether alternative readings can coexist or whether the artifact_reading''s dominance is structural.').

omega_variable(
    young_generation_identity_lock_trajectory,
    'Does the younger generation''s identity_locked exit option remain stable as they reach economic independence, or do increasing numbers leave the community when they can choose (Rumspringa outcome and post-Rumspringa return patterns)?',
    'Longitudinal demographic data on Rumspringa outcomes for cohorts exposed to the tightening artifact enforcement (higher suppression_requirement over time). Do retention rates decline for younger generations? Do returners from Rumspringa who adopt forbidden technologies face escalating discipline?',
    'If younger members are leaving permanently at higher rates, the identity-lock is weakening and the constraint faces declining support from the generation that would inherit authority. This suggests the suppression is unsustainable and the constraint is transitioning toward piton status (maintained theatrically despite declining functional support). If retention remains stable, the identity-lock persists and the constraint maintains coercive force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(young_generation_identity_lock_trajectory, empirical, 'Whether identity-lock is remaining stable in younger generations or weakening.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__artifact_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__artifact_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement_basis(gela_tr_t0, observed).
narrative_ontology:measurement(gela_tr_t8, gelassenheit_separation__artifact_reading, theater_ratio, 8, 0.52).
narrative_ontology:measurement_basis(gela_tr_t8, observed).
narrative_ontology:measurement(gela_tr_t16, gelassenheit_separation__artifact_reading, theater_ratio, 16, 0.56).
narrative_ontology:measurement_basis(gela_tr_t16, observed).
narrative_ontology:measurement(gela_tr_t24, gelassenheit_separation__artifact_reading, theater_ratio, 24, 0.59).
narrative_ontology:measurement_basis(gela_tr_t24, observed).
narrative_ontology:measurement(gela_tr_t32, gelassenheit_separation__artifact_reading, theater_ratio, 32, 0.61).
narrative_ontology:measurement_basis(gela_tr_t32, observed).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__artifact_reading, theater_ratio, 40, 0.62).
narrative_ontology:measurement_basis(gela_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__artifact_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement_basis(gela_be_t0, observed).
narrative_ontology:measurement(gela_be_t8, gelassenheit_separation__artifact_reading, base_extractiveness, 8, 0.7).
narrative_ontology:measurement_basis(gela_be_t8, observed).
narrative_ontology:measurement(gela_be_t16, gelassenheit_separation__artifact_reading, base_extractiveness, 16, 0.75).
narrative_ontology:measurement_basis(gela_be_t16, observed).
narrative_ontology:measurement(gela_be_t24, gelassenheit_separation__artifact_reading, base_extractiveness, 24, 0.79).
narrative_ontology:measurement_basis(gela_be_t24, observed).
narrative_ontology:measurement(gela_be_t32, gelassenheit_separation__artifact_reading, base_extractiveness, 32, 0.81).
narrative_ontology:measurement_basis(gela_be_t32, observed).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__artifact_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement_basis(gela_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__artifact_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement_basis(gela_su_t0, observed).
narrative_ontology:measurement(gela_su_t8, gelassenheit_separation__artifact_reading, suppression_requirement, 8, 0.82).
narrative_ontology:measurement_basis(gela_su_t8, observed).
narrative_ontology:measurement(gela_su_t16, gelassenheit_separation__artifact_reading, suppression_requirement, 16, 0.84).
narrative_ontology:measurement_basis(gela_su_t16, observed).
narrative_ontology:measurement(gela_su_t24, gelassenheit_separation__artifact_reading, suppression_requirement, 24, 0.86).
narrative_ontology:measurement_basis(gela_su_t24, observed).
narrative_ontology:measurement(gela_su_t32, gelassenheit_separation__artifact_reading, suppression_requirement, 32, 0.88).
narrative_ontology:measurement_basis(gela_su_t32, observed).
narrative_ontology:measurement(gela_su_t40, gelassenheit_separation__artifact_reading, suppression_requirement, 40, 0.89).
narrative_ontology:measurement_basis(gela_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__artifact_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__artifact_reading, 0.12).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__consequence_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__principle_reading).

% DUAL FORMULATION NOTE:
% The Gelassenheit separation kernel ('what does separation from worldly entanglement mean?') has been decomposed into three constraint stories, each instantiating one reading of the kernel. The artifact_reading (this story) enforces visible distinction on appearance grounds; the consequence_reading evaluates separation by effects on community practices; the principle_reading evaluates separation by structural isolation from economic systems. The three readings coexist within community discourse but are structurally different constraints with different ε values and enforcement logics. They are linked here to enable cross-reading analysis and to track which reading dominates in which institutional contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gelassenheit_separation__artifact_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
