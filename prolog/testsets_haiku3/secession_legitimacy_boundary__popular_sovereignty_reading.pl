% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__popular_sovereignty_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__popular_sovereignty_reading
 *   human_readable: Provincial Majority Sovereignty and Unilateral Secession Right (Popular Sovereignty Reading)
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint embodies one reading of a contested political kernel: the
 *   secession legitimacy boundary. Under the popular_sovereignty_reading, a
 *   provincial/regional majority has the ultimate right to exit a federal
 *   union through a democratic referendum, and that referendum result is
 *   self-legitimating — it requires no validation from a federal court,
 *   constitutional amendment, or international consent. This reading
 *   prioritizes popular will and territorial self-determination over
 *   constitutional text, institutional continuity, or minority rights. The
 *   constraint is CLAIMED as tangled_rope (genuine coordination function:
 *   democratic expression of regional identity; asymmetric extraction:
 *   federal minorities and minorities within the province lose voice and
 *   territory). Metrics show moderate-to-high extraction and suppression
 *   because the majority's exit right inherently subordinates minority
 *   preferences within the province and overrides federal-level actors' voice
 *   in territorial redistribution. The measurement series tracks rising
 *   theater_ratio as referenda become increasingly about symbolic assertion
 *   of sovereignty rather than negotiation, and suppression rises as federal
 *   institutions (and minority opponents within the province) face
 *   suppression of their exit options and of constitutional paths to
 *   challenge the result.
 *
 * KEY AGENTS:
 *   - provincial_majority_coalition: Dense political coalition that would vote for secession; defines itself around regional identity and perceived federal extraction
 *   - independence_movement_leadership: Organized political actors who frame secession as legitimate, campaign for referenda, and claim to speak for the provincial majority
 *   - federal_authority_institutions: Constitutional courts, federal legislature, executive branches that operate under a prior reading of the federal union as indissoluble or amendment-constrained
 *   - provincial_minorities: Residents of the province who oppose secession or favor remaining in the federal union; face suppression of their exit option
 *   - federal_minorities_in_province: Groups (ethnic, religious, ideological) whose identity, rights, or interests are rooted in federal-union-wide institutions; would lose access post-secession
 *   - creditor_states_and_markets: International actors with contractual or political claims on the federation; face loss of collateral, jurisdiction, or treaty relations post-secession
 *   - treaty_holding_nations: Indigenous or historical nations with prior treaty commitments to the federal union; would be bound by provincial majority referendum under this reading (contested by the treaty_primacy_reading)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, 0.68).
domain_priors:suppression_score(secession_legitimacy_boundary__popular_sovereignty_reading, 0.71).
domain_priors:theater_ratio(secession_legitimacy_boundary__popular_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__popular_sovereignty_reading, "Provincial Majority Sovereignty and Unilateral Secession Right (Popular Sovereignty Reading)").
narrative_ontology:topic_domain(secession_legitimacy_boundary__popular_sovereignty_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__popular_sovereignty_reading, '8da39869-06cf-422e-b3c5-7589ea2fa9ce').
narrative_ontology:cs_kernel_codification('8da39869-06cf-422e-b3c5-7589ea2fa9ce', distributed).
narrative_ontology:cs_authority_grounding('8da39869-06cf-422e-b3c5-7589ea2fa9ce', distributed).
narrative_ontology:cs_reading_relation('8da39869-06cf-422e-b3c5-7589ea2fa9ce', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('8da39869-06cf-422e-b3c5-7589ea2fa9ce', secession_legitimacy_boundary__grievance_threshold_reading, influences).
narrative_ontology:cs_reading_relation('8da39869-06cf-422e-b3c5-7589ea2fa9ce', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('8da39869-06cf-422e-b3c5-7589ea2fa9ce', foundational, provincial_majority_ultimate_sovereignty).
narrative_ontology:cs_axiom_status(provincial_majority_ultimate_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('8da39869-06cf-422e-b3c5-7589ea2fa9ce', provincial_majority_ultimate_sovereignty, deontological).
narrative_ontology:cs_axiom('8da39869-06cf-422e-b3c5-7589ea2fa9ce', foundational, referendum_result_self_legitimating).
narrative_ontology:cs_axiom_status(referendum_result_self_legitimating, holdable).
narrative_ontology:cs_axiom_grounding('8da39869-06cf-422e-b3c5-7589ea2fa9ce', referendum_result_self_legitimating, conventional).
narrative_ontology:cs_reference_frame('8da39869-06cf-422e-b3c5-7589ea2fa9ce', federal_union_dissoluble_by_provincial_popular_will).
narrative_ontology:cs_drift_state('8da39869-06cf-422e-b3c5-7589ea2fa9ce', contemporary_international_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8da39869-06cf-422e-b3c5-7589ea2fa9ce', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority_coalition).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, independence_movement_leadership).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, federal_authority_institutions).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minorities).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, federal_minorities_in_province).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, creditor_states_and_markets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, treaty_holding_nations).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__popular_sovereignty_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_self_determination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Forms the electoral coalition that votes for secession referenda and advocates for provincial independence. Frames the exit as liberation from federal extraction or cultural suppression. Controls the referendum mechanism and the independence movement's narrative after a successful vote. Their exit is not suppressed — they exercise it.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority_coalition, agenda_setter,
    organized, generational, arbitrage, regional).

% Political, intellectual, and organizational leaders of the secession movement. Gain power, legitimacy, and institutional position from claiming sovereignty. Craft the referendum language and campaign for the vote. Their authority is contingent on the reading's validity (if courts or international bodies reject it, their legitimacy erodes). They can exit the constraint by emigrating, but their power derives from staying within it.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, independence_movement_leadership, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__popular_sovereignty_reading, independence_movement_leadership, agenda_setter).

% Residents of the province who oppose secession or prefer remaining in the federal union. Their vote in the referendum is suppressed by majority rule; their exit option (remaining in the federation) is removed if secession succeeds. They bear the loss of federal-level minority protections and institutional access. Post-secession, they face a government constituted by the secession coalition, against their expressed preference.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minorities, payer,
    powerless, generational, constrained, regional).

% Federal courts, legislatures, and executive bodies that operate under a constitutional framework treating the union as (they claim) indissoluble or amendment-constrained. Their legitimate authority to adjudicate or negotiate exit is suppressed by the referendum mechanism; the reading subordinates their constitutional seat to the provincial majority's will. They cannot exit their institutional identity (they ARE federal institutions) and cannot change the secession outcome after a referendum (their review is suppressed).
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_authority_institutions, payer,
    institutional, generational, identity_locked, national).

% Groups (ethnic, religious, ideological) whose primary identity and institutional access are rooted in federal-union-wide structures (federal courts, national institutions, pan-union advocacy networks). Post-secession, they lose that institutional framework and must reorganize within the province's borders or emigrate. Their exit option (remaining a member of the federal union) is tied to their place of residence, which the referendum removes. Identity as federal-union members is suppressed; emigration is their only exit.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_minorities_in_province, payer,
    powerless, biographical, identity_locked, national).

% States with treaty obligations from the federation, creditors holding federal debt, or investment contracts with federal entities. A unilateral secession restructures or severs these commitments without their consent. They can impose economic penalties (sanctions, credit denial) but cannot prevent the secession if the province's majority determines to proceed. Their contractual exit option (demanding performance) is constrained by the province's new sovereignty claim.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, creditor_states_and_markets, payer,
    powerful, biographical, constrained, global).

% Indigenous or historical nations with treaty commitments to the federal union (prior to the province's founding or incorporation). Under this reading, a provincial majority referendum subordinates these prior commitments; the nations lose their federal-union-wide treaty standing and must negotiate with the province. They are excluded from the referendum mechanism (not a 'majority within the province' if they control territory or claim separate jurisdiction). Their trapped exit means they cannot remain in the federation if the province exits; they must negotiate a new status post-secession or face internal conflict.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, treaty_holding_nations, payer,
    organized, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__popular_sovereignty_reading, treaty_holding_nations, excluded).

% Groups in OTHER provinces or federal districts who would be affected by a province's exit — loss of shared federal governance, loss of access to federal-union-wide advocacy resources, loss of pan-union institutional coordination. They are excluded from the seceding province's referendum but affected by it. Their voice in the federal union's integrity is suppressed by the reading's subordination of federal authority to provincial will.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_minorities_and_marginalized_groups, excluded,
    powerless, biographical, trapped, national).

% International law scholars, constitutional theorists, historians, and analysts who study the legitimacy of secession and federal dissolution. They measure the constraint's operation against norms of self-determination, minority rights, international law, and institutional continuity. They can publish analysis and shape elite discourse but cannot directly alter the constraint's operation.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, academic_and_legal_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority_coalition).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Expresses provincial identity and regional will through democratic referendum, providing a mechanism for translating majority sentiment into a binding claim of territorial sovereignty. Coordinates the independence movement's political base and provides legitimate outlet for grievances without violence.
% TRANSFER_FUNCTION: Moves territorial control, sovereignty, and institutional authority from federal to provincial hands; extracts federal minorities' federal-union-wide institutional access; extracts minorities within the province's voice in determining their own governance; extracts creditor states' contractual claims and treaty-holding nations' federal-union-level treaty standing.
% ABSENT_VOICES: Provincial minorities who oppose secession are excluded from the beneficiary set and their voice is suppressed by majority rule. Federal minorities in the province, who would advocate for retaining federal-union-wide rights, are similarly excluded. Treaty-holding nations (if present) are excluded from the referendum mechanism if they claim separate jurisdiction. Federal-level minorities outside the province have no voice in the seceding province's referendum but bear consequences. These groups would argue for constitutional amendment processes, minority protections, treaty renegotiation, or federal authority to adjudicate the legitimacy of exit — all positions the popular_sovereignty_reading suppresses.
% DISAPPEARANCE_RATIONALE: If this reading disappeared (replaced by the constitutional_impossibility_reading, for instance), federal authority would reassert control over territorial exit, provinces would lose unilateral secession rights, minorities would retain federal-union-wide protections, and creditor states would retain treaty standing. The political landscape would shift from majority-driven exit toward negotiated constitutional amendment or international arbitration. Territories currently mobilized around secession would reorganize around federal-level politics.
% FOUNDING_PROBLEM: Provincial regions experience federal governance structures that they perceive as extracting their resources, suppressing their cultural identity, or failing to represent their interests. They seek a mechanism to express their will and exit if the federal arrangement no longer serves them.
% FOUNDING_PROBLEM_CORROBORATION: Provincial independence movements attest the founding problem is live. Federal institutions dispute that federal governance is inherently extractive and point to federal transfer programs, minority protections, and shared prosperity as evidence the union serves provincial interests. Academic observers note that grievances exist but are debated (some provinces report extraction, others report net transfers; some see cultural suppression, others see multicultural coexistence). The corroboration is mixed: only the independence coalition unanimously affirms the founding problem. Neutral external observers (international law scholars, historical analysts) note the problem exists as a PERCEIVED condition (majorities do believe they are extracted from) but is contested as a factual condition (whether federal transfers, institutional bias, or resource use support the perception varies by jurisdiction and time period).
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__popular_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 by interval end because the reading privileges provincial majority preferences absolutely, extracting from (1) federal authority institutions' legitimate seat at the bargaining table, (2) minorities within the province who lose voice, (3) federal minorities whose federal-union-wide identity and rights are subordinated, and (4) creditor states who face unilateral treaty/debt restructuring. Suppression is higher (0.71) because the reading's persistence depends on actively suppressing federal courts' review authority, minority exit (they cannot detach their territory and remain in the federation), and constitutional amendment paths. Theater_ratio rises from 0.28 to 0.42 over the interval because referenda shift from negotiation tools (early) to assertion of sovereignty (late); the performative function (affirming identity, symbolizing majority will) increasingly crowds out the instrumental function (communicating preferences to federal negotiators). Accessibility_collapse is moderate (0.58) because provinces CAN remain in the federation (no alternatives collapse completely) but the referendum mechanism itself collapses the exit option for minorities and makes federal constitutional paths effectively inaccessible (courts are suppressed). Resistance is high (0.72) because federal institutions, minorities within the province, and creditor states actively resist the reading via constitutional law, appeals to international norms, and economic pressure.
 *
 * PERSPECTIVAL GAP:
 *   The provincial_majority_coalition and independence_movement_leadership should compute this as rope or light tangled_rope (genuine coordination of regional identity and expression of will; they see minimal extraction). The federal_authority_institutions and provincial_minorities should compute it as snare or heavy tangled_rope (the majority's sovereignty is achieved by suppressing their constitutional standing and exit options). The creditor_states and treaty_holding_nations have even starker divergence: they compute it as snare with no coordination function at all (unilateral extraction of their contractual claims and treaty priority). The engine's per-seat computation from the structural data (beneficiary/victim declaration + power + exit_options + directionality) will surface these gaps automatically; the authored claim (tangled_rope) does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The provincial_majority_coalition holds directionality near 0.0 (full beneficiary): they set the agenda (yes, via referendum mechanism), they gain sovereignty and territorial control, and their exit option is not suppressed — they choose exit. The independence_movement_leadership similarly sits near 0.0 because they capture the legitimacy narrative and political power post-secession. Federal_authority_institutions have directionality near 1.0 (full target): their constitutional authority is subordinated, they lose jurisdiction, their voice in territorial matters is suppressed by the referendum mechanism. Provincial_minorities and federal_minorities_in_province both have high directionality (0.7–0.9 range): they pay in suppressed voice, lost federal-union-wide identity and rights, and no exit option (the province exits, they are carried with it). Creditor_states are targets (1.0 range) because they bear unilateral treaty restructuring. The reading_relations and axioms in cs_structure encode the logical structure that distinguishes this reading from its siblings and creates these directional divergences.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows potential mandatrophy: the founding problem ('federal overreach extracts from provinces; we need self-determination') is CONTESTED in status (federal defenders deny overreach; minorities in the province deny they need liberation). The disappearance_verdict would be world_rearranges (the constraint's operation is central to territorial stability and federal legitimacy). But theater_ratio rising to 0.42 suggests the original coordination function (expression of regional will, negotiating leverage) is increasingly ceremonial — what persists is the claim to sovereignty divorced from the federal integration it once negotiated within. The constraint may be approaching mandatrophy: the founding problem is dead (modern federations have mechanisms to address regional grievance) but the arrangement persists as identity affirmation and political leverage, not as coordination of a live problem. This is NOT yet resolved mandatrophy because the beneficiary coalition still derives power from the threat of exercise; true piton status would require the sovereignty claim to become pure ceremony with no credible exit option behind it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    axiom_popular_sovereignty_grounding,
    'Is popular sovereignty grounded in an empirically falsifiable claim about political legitimacy, or is it a deontological commitment that cannot be undermined by structural evidence?',
    'Examine whether the reading''s adherents would abandon popular_sovereignty_supremacy if shown empirical evidence (economic disaster, minority rights violations post-secession, treaty breach cascade). If the axiom persists despite contrary evidence, it is deontological; if it recedes with evidence of dysfunction, it is empirically contingent.',
    'If empirically_contingent and evidence of institutional failure accumulates, the axiom becomes foreclosed-candidate; if deontological, axiom_overriding as a drift_state direction becomes inapplicable (only authority_erosion applies). Classification consequence: the latter case resists foreclosure through accumulation of contrary drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_popular_sovereignty_grounding, conceptual, 'Is the popular sovereignty axiom empirically contingent or deontologically grounded?').

omega_variable(
    referendum_binding_mechanism,
    'What makes a referendum result ''self-legitimating''? Is it the act of voting itself (participation as legitimacy), the numerical threshold (majority as legitimacy), or temporal/procedural conditions (duration, oversight, informed consent)?',
    'Examine actual referendum design in jurisdictions claiming popular sovereignty — does the reading treat a 50%+1 vote as legitimating? A supermajority? Participation thresholds? Review constitutional courts'' rulings on what votes suffice for major territorial change.',
    'Different thresholds and conditions embed different extraction profiles: a low threshold (50%+1) extracts more from the minority; a high threshold (supermajority) or turnout requirement narrows the beneficiary set and raises suppression (minority veto attempts). Threshold choice changes who the payer is.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(referendum_binding_mechanism, empirical, 'What procedural or numerical conditions make a referendum self-legitimating in this reading?').

omega_variable(
    majority_composition_instability,
    'Does the ''provincial majority'' that holds sovereignty remain the same coalition across multiple referenda, or is it a shifting coalition? Can a first referendum affirm secession, and a second referendum (years later, with demographic change) reverse it?',
    'Study sequences of referenda in contexts where the reading is applied (Quebec 1995, 1980; Catalonia 2017; Scotland 2014, 2023). Examine whether the reading treats sovereignty as permanently conferred after one vote or as revocable by future majorities.',
    'If sovereignty is permanently granted by one referendum, the constraint extracts from the post-secession minority and locks in the founding majority''s power. If revocable, the constraint becomes cyclical — alternating extraction from different temporal coalitions, which is still extractive but changes the victim set and creates intermittent reinforcement dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majority_composition_instability, empirical, 'Is provincial sovereignty granted permanently by one referendum or revocable by future majorities?').

omega_variable(
    reading_vs_sibling_boundary_contestation,
    'Which features of the kernel text (if written) or the historical constitutional compromise are claimed as evidence for popular_sovereignty_reading versus constitutional_impossibility_reading? Is there any reading of the founding text that both sibling readings accept?',
    'Compare original constitutional documents, court interpretations, and settler-versus-successor-state practices (the US, Canada, EU, USSR dissolutions). Identify which textual evidence each reading cites and which evidence is contested.',
    'If the founding text is genuinely ambiguous (supports both readings), the kernel''s codification is `distributed` rather than `formalized`, and the coexists_with relation is more stable. If one reading claims textual support the other rejects, the relation may trend toward influences or foreclose if textual authority is eventually adjudicated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_sibling_boundary_contestation, conceptual, 'Is the founding kernel text genuinely ambiguous or does it prefer one reading over another?').

omega_variable(
    indigenous_treaty_subordination,
    'Under this reading, if a provincial majority votes for secession against the will of treaty-holding nations within the province, do those nations retain the right to remain within the federal union (treaty_primacy_reading''s claim), or does provincial majority sovereignty override their treaty rights?',
    'Examine secession scenarios in territories with overlapping indigenous jurisdictions (Québec with Cree/Inuit, Catalonia with Basque country precedent, Canada post-Meech Lake). Does the popular_sovereignty_reading explicitly subordinate treaty rights to referendum results, or does it treat them as external to the sovereignty boundary?',
    'If treaties are subordinate: the constraint extracts from treaty nations, suppresses their exit options, and classifies as snare-flavored for them (referendum overrides prior commitments). If treaties are external: the constraint only applies to the non-treaty population, boundaries are porous, and the victim set shrinks. The reading''s ε and classification depend on this resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_treaty_subordination, conceptual, 'Does provincial majority sovereignty override prior indigenous treaty rights in this reading?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__popular_sovereignty_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(sece_tr_t0, observed).
narrative_ontology:measurement(sece_tr_t5, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(sece_tr_t5, observed).
narrative_ontology:measurement(sece_tr_t10, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(sece_tr_t10, observed).
narrative_ontology:measurement(sece_tr_t15, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(sece_tr_t15, observed).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(sece_tr_t20, observed).
narrative_ontology:measurement(sece_tr_t25, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(sece_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(sece_be_t0, observed).
narrative_ontology:measurement(sece_be_t5, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(sece_be_t5, observed).
narrative_ontology:measurement(sece_be_t10, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(sece_be_t10, observed).
narrative_ontology:measurement(sece_be_t15, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(sece_be_t15, observed).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(sece_be_t20, observed).
narrative_ontology:measurement(sece_be_t25, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(sece_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(sece_su_t0, observed).
narrative_ontology:measurement(sece_su_t5, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(sece_su_t5, observed).
narrative_ontology:measurement(sece_su_t10, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(sece_su_t10, observed).
narrative_ontology:measurement(sece_su_t15, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(sece_su_t15, observed).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(sece_su_t20, observed).
narrative_ontology:measurement(sece_su_t25, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(sece_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__popular_sovereignty_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__popular_sovereignty_reading, 0.15).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% The secession_legitimacy_boundary kernel decomposes into four structurally distinct constraints, each embodying a different answer to 'what legitimates unilateral exit from a federal union.' The popular_sovereignty_reading (this file) claims democratic majorities within provincial boundaries hold ultimate authority. The constitutional_impossibility_reading claims the federation's founding text precludes unilateral exit. The grievance_threshold_reading claims structural federal injustice must cross a threshold, regardless of constitutional text. The treaty_primacy_reading claims indigenous treaty rights supersede both federal and provincial authority. These are NOT alternative measurements of one constraint — they are different constraints with different ε values, different beneficiary/victim structures, different types. The kernel itself is the contested commitment (the legitimacy rule for territorial exit); each reading instantiates that kernel differently. All four stories link to each other via network.affects_constraints because they dispute a common boundary (what makes secession legitimate) and share the same stakeholder set (federal institutions, provinces, minorities, treaty nations, creditor states).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secession_legitimacy_boundary__popular_sovereignty_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
