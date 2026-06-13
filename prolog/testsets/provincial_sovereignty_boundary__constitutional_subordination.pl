% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__constitutional_subordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__constitutional_subordination, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: provincial_sovereignty_boundary__constitutional_subordination
 *   human_readable: Constitutional Subordination of Provinces to Federal Authority
 *   domain: political_economy/federalism
 *
 * SUMMARY:
 *   The constitutional subordination reading asserts that provinces are
 *   creatures of the federal constitution—they derive all their authority
 *   from the 1867 Constitution Act and possess no inherent or
 *   pre-constitutional sovereignty. Under this reading, exit requires federal
 *   consent (legally impossible unilaterally); federal spending power and
 *   climate policy are legitimate exercises of constitutional authority; and
 *   separatism is a constitutional non-event (has no legal standing, only
 *   political salience). This is the reading endorsed by the Canadian
 *   judiciary and the federal government. It is contested by two sibling
 *   readings: compact federalism (provinces are sovereign parties to a
 *   negotiable compact) and resource sovereignty primacy (provincial
 *   ownership of natural resources grounds absolute territorial sovereignty).
 *   This story instantiates ONLY the constitutional subordination reading as
 *   a clean constraint with stable ε. The sibling readings are separate
 *   constraints with their own ε values and structural dynamics. The contest
 *   between readings is routed to omega variables (irreducible uncertainty
 *   about the kernel's true nature) rather than embedded in this constraint's
 *   metrics.
 *
 * KEY AGENTS:
 *   - federal_government: Agenda-setter, enforces the constitutional reading through policy and litigation. Institutional power; arbitrage-class exit (can reshape the Constitution but holds the default position). Structural beneficiary of the subordination reading.
 *   - exit_seeking_provinces: Payer. Powerful but trapped in the constitutional frame. Would exit if able; constrained by the legal denial of unilateral exit. Structural victim of the subordination reading.
 *   - resource_dominant_provinces: Dual-positioned payer/beneficiary. Benefit from equalization transfers but pay through resource policy subordination and exit constraint. Constrained exit (could renegotiate but not unilaterally withdraw). Ambivalent about the reading.
 *   - central_canadian_provinces: Beneficiary. Mobile exit options (net receivers of redistribution; less resource claim). Benefit from federal subordination that protects inter-provincial market and wealth transfer.
 *   - indigenous_nations: Excluded. Identity-locked. Their sovereignty claim predates and contradicts the entire provincial-federal frame. Structural absence from the reading is enforced by both federal and provincial veto.
 *   - canadian_judiciary: Dual-seated (observer + agenda-setter). Interprets and enforces the constitutional subordination reading through landmark decisions (Reference re Secession of Quebec). Observer seat: analytical seat reading the constitution. Agenda-setter seat: enforcement through jurisprudence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, 0.68).
domain_priors:suppression_score(provincial_sovereignty_boundary__constitutional_subordination, 0.72).
domain_priors:theater_ratio(provincial_sovereignty_boundary__constitutional_subordination, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, extractiveness, 0.68).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__constitutional_subordination, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__constitutional_subordination, "Constitutional Subordination of Provinces to Federal Authority").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__constitutional_subordination, "political_economy/federalism").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__constitutional_subordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__constitutional_subordination, '5a580a13-5e9c-48b3-a928-55cb2b0fd2cf').
narrative_ontology:cs_kernel_codification('5a580a13-5e9c-48b3-a928-55cb2b0fd2cf', fixed_text).
narrative_ontology:cs_authority_grounding('5a580a13-5e9c-48b3-a928-55cb2b0fd2cf', lineage).
narrative_ontology:cs_interpretation_layer_present('5a580a13-5e9c-48b3-a928-55cb2b0fd2cf').
narrative_ontology:cs_reading_relation('5a580a13-5e9c-48b3-a928-55cb2b0fd2cf', provincial_sovereignty_boundary__compact_federalism, coexists_with).
narrative_ontology:cs_reading_relation('5a580a13-5e9c-48b3-a928-55cb2b0fd2cf', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('5a580a13-5e9c-48b3-a928-55cb2b0fd2cf', foundational, federal_supremacy_from_constitutional_act).
narrative_ontology:cs_axiom_status(federal_supremacy_from_constitutional_act, holdable).
narrative_ontology:cs_axiom_grounding('5a580a13-5e9c-48b3-a928-55cb2b0fd2cf', federal_supremacy_from_constitutional_act, conventional).
narrative_ontology:cs_axiom('5a580a13-5e9c-48b3-a928-55cb2b0fd2cf', foundational, provincial_sovereignty_is_derivative).
narrative_ontology:cs_axiom_status(provincial_sovereignty_is_derivative, holdable).
narrative_ontology:cs_axiom_grounding('5a580a13-5e9c-48b3-a928-55cb2b0fd2cf', provincial_sovereignty_is_derivative, deontological).
narrative_ontology:cs_reference_frame('5a580a13-5e9c-48b3-a928-55cb2b0fd2cf', constitutional_supremacy_doctrine).
narrative_ontology:cs_drift_state('5a580a13-5e9c-48b3-a928-55cb2b0fd2cf', contemporary_climate_federalism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5a580a13-5e9c-48b3-a928-55cb2b0fd2cf', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, federal_government).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, national_coordination_function).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, exit_seeking_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, resource_dominant_provinces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, resource_dominant_provinces).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, central_canadian_provinces).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, federal_supremacy_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, constitutional_indivisibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the constitutional reading through policy, litigation, and constitutional interpretation. Maintains that provinces are subordinate creatures of the constitution with no inherent sovereignty. Defends federal spending power, federal climate authority, and the federal veto over provincial exit. Collects legitimacy from national union, territorial integrity, and the coordination benefits of centralized fiscal redistribution. Can reshape the Constitution through federal-provincial agreement, but holds the default interpretation and enforcement power.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Seek independence or substantial sovereignty renegotiation but are legally barred from unilateral exit by the subordination reading. Must negotiate exit terms with the federal government, which holds the default no. Face fiscal and legal costs of subordination (cannot unilaterally raise capital through resource monetization, must accept federal climate mandates, lack the legal standing to secede). Politically mobilized to contest the subordination reading but constrained by its legal force.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, exit_seeking_provinces, payer,
    powerful, generational, trapped, national).

% Own natural resources (oil, gas, minerals) under section 92A but face federal override through spending power, climate policy, and interprovincial trade rules. Receive equalization transfers that redistribute their resource wealth but resent the subordination of resource control. Benefit from national market protection and redistribution but pay through policy constraint. Dual-positioned: neither full payers nor full beneficiaries; leverage resource ownership to renegotiate but cannot exit unilaterally.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, resource_dominant_provinces, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__constitutional_subordination, resource_dominant_provinces, beneficiary).

% Receive net equalization transfers and benefit from federal coordination of the national market. Have lower exit costs than resource-dominant provinces because they benefit from the subordination arrangement and have weaker resource claims. Politically aligned with federal authority structure because it protects their economic position and ensures wealth transfers from resource-rich regions. Mobile exit because they could survive outside the federation but choose not to because they benefit from it.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, central_canadian_provinces, beneficiary,
    powerful, generational, mobile, national).

% Are structurally absent from the provincial-federal dyad and not seated in constitutional negotiations. Their pre-constitutional sovereignty claim contradicts both federal supremacy and provincial subordination. Face federal or provincial veto on resource extraction, jurisdiction, and self-determination. Identity-locked: their territorial and cultural claims are inseparable from the land and cannot be satisfied by exit within the federal frame. The subordination reading enforces their exclusion by treating indigenous nations as subject to federal or provincial authority, not as independent sovereigns.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, indigenous_nations, excluded,
    moderate, generational, identity_locked, regional).

% Interprets and enforces the constitutional subordination reading through landmark cases (Reference re Secession of Quebec, 1998; Reference re Senate Reform, 2014). The Supreme Court has consistently affirmed federal supremacy and the impossibility of unilateral provincial exit. Dual-seated: observer (analytically interprets the constitution) and agenda-setter (enforces the interpretation as binding law). Their interpretation gives the constraint the force of law, not mere policy, and makes provinces legally subordinate rather than just politically dependent.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, canadian_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__constitutional_subordination, canadian_judiciary, observer).

% Contest the subordination reading by advancing the alternative reading that confederation was a compact among sovereign provinces that retain residual sovereignty. Includes Quebec sovereignty movements, some Western separatists, and constitutional scholars. Cannot overturn the judiciary's subordination reading but challenge its legitimacy through political mobilization and scholarly critique. Their alternative reading would decompose the federal constraint and enable unilateral provincial exit.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, compact_federalism_advocates, observer,
    powerful, generational, analytical, national).

% Advance the reading that section 92A (provincial ownership of natural resources) grounds absolute territorial sovereignty independent of federal constitution. Primarily resource-dominant provinces (Alberta, Saskatchewan) asserting resource control as the ground of sovereignty. Challenge the subordination reading by claiming resource ownership is the foundational sovereignty atom, not a derivative right granted by the constitution. Would dissolve federal climate authority and interprovincial resource trade constraints if politically ascendant.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, resource_sovereignty_advocates, observer,
    powerful, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single unified nation-state with centralized authority over interprovincial commerce, fiscal redistribution, and national infrastructure. Solves the collective-action problem of provinces free-riding on national public goods (defense, currency, pan-Canadian infrastructure) and prevents a race-to-the-bottom in regulatory standards. Federal spending power enables cross-provincial equalization transfers and national climate policy coordination that individual provinces would not achieve through decentralized bargaining.
% TRANSFER_FUNCTION: Extracts fiscal capacity (federal taxation authority exceeds provincial needs under the vertical fiscal imbalance), policy authority (federal jurisdiction expands through spending power interpretation), and sovereignty claims (provinces cede the right to unilateral exit and must negotiate constitutional change through federal veto) from provinces to the federal government. Redistributes some extracted capacity to poorer provinces through equalization transfers and reinvests it in national infrastructure and climate commitments. Resource-dominant provinces pay most (resource control subordinated); exit-seeking provinces pay the cost of exit denial; central Canadian provinces receive net redistribution.
% ABSENT_VOICES: Indigenous nations, whose pre-constitutional sovereignty claims predate and contradict the entire provincial-federal frame, are excluded from constitutional negotiations and have no seat in the dyadic structure. Compact federalism advocates (Quebec sovereignty movement, Western separatists) lack formal constitutional standing to contest subordination—their challenge is political, not legal. Conservative provincial governments that reject federal climate authority are included but marginalized (lose federal funding for non-compliance). Private sector entities (natural resource corporations) are stakeholders in resource policy but lack formal seats in constitutional structures.
% DISAPPEARANCE_RATIONALE: If constitutional subordination vanished overnight—if provinces acquired inherent sovereignty and the legal right to unilateral exit—the federation would face immediate dissolution or radical reorganization. Exit-seeking provinces would initiate secession proceedings; resource-dominant provinces would renegotiate resource control and exit terms; equalization transfers would cease; the national market would fragment; climate policy would devolve to provincial patchworks. The Canadian political and economic union would dissolve or become a much looser confederation. Central Canadian provinces would lose their equalization benefits and would face new trade barriers. The federal government would lose territorial integrity and fiscal leverage. The constraint's disappearance is not a marginal adjustment—it is the unraveling of the state form itself.
% FOUNDING_PROBLEM: The Dominion of Canada required a structure to unite four British colonies (Ontario, Quebec, Manitoba, British Columbia) with divergent interests, economic endowments, and political cultures into a single nation-state. Confederation had to balance provincial autonomy (colonies wanted to retain control over local affairs) with national unity (a single currency, common defense, interprovincial market required central authority). The constitutional question was left unresolved: did confederation create a new sovereign nation with provinces as subordinate administrative creatures, or was it a compact among pre-existing sovereign polities that retained residual sovereignty and the right to renegotiate or exit?
% FOUNDING_PROBLEM_CORROBORATION: The federal government and the Canadian Supreme Court (Reference re Secession of Quebec, 1998; Reference re Senate Reform, 2014) attest that confederation created a new sovereign nation with provinces as creatures of the constitution. The Court explicitly held that provinces have no inherent sovereignty and no legal right to unilateral secession. Quebec and Western provincial governments, constitutional scholars aligned with compact federalism (Peter H. Russell, James Tully, Gordon Gibson), and indigenous nations attest that the founding problem remains unresolved—that confederation was a negotiated compact and provinces retain residual sovereignty or that indigenous sovereignty predates the entire frame. The contest is not resolved by external arbitration: the UN does not adjudicate internal constitutional questions; the UK (former imperial authority) recognized Canadian sovereignty but did not settle the federal-provincial question. The mismatch between the legal ruling (subordination is settled) and the political contestation (provinces continue to assert sovereignty claims) is the mark of an unresolved kernel.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__constitutional_subordination, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__constitutional_subordination, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__constitutional_subordination, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__constitutional_subordination, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because exit-seeking and resource-dominant provinces bear costs (fiscal subordination, policy constraint, legal impossibility of exit) without being able to renegotiate the arrangement unilaterally. Suppression is substantial (0.72) because the enforcement machinery is two-tiered: constitutional law (courts uphold federal supremacy, making exit legally null) and fiscal dependency (provinces depend on federal transfers and cannot escape the union without federal negotiation). Theater is moderate (0.42): the constraint's legitimacy narrative (national unity, equalization, coordination) is real and functional, but an increasing share of enforcement activity over the interval is devoted to defending federal veto power itself rather than to the coordination benefits—provinces increasingly contest the subordination reading, and the suppressive machinery must work harder to maintain it. Measurements are taken on a shared time grid (t0=0, tn=50) at six points per metric. The rising trajectory in all three metrics reflects accumulating strain: extraction rises as resource tensions intensify; suppression rises as provinces mount legal and political challenges; theater rises as performative legitimacy work (constitutional conferences, nation-building rhetoric) grows relative to actual functional coordination gains.
 *
 * PERSPECTIVAL GAP:
 *   The federal government and central Canadian provinces experience this constraint as coordinating (enables redistribution and national policy). Exit-seeking and resource-dominant provinces experience it as extractive (denies them sovereignty and exit rights). The engine computes these divergent classifications from the structural data: agenda-setter and beneficiary seats (federal, central Canada) derive low directionality (d toward beneficiary end), while payer seats (exit-seeking, resource-dominant provinces) derive high directionality (d toward target end). The constraint type per seat will diverge: likely tangled_rope or snare from the payer seats, rope from the beneficiary seats. This divergence is the measurement the constraint story exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government: d ≈ 0.05 (full beneficiary—collects legitimacy, retains policy authority, fears exit but controls the constitutional frame). Exit-seeking provinces: d ≈ 0.88 (near full target—trapped by law, bear the cost of exit denial, no arbitrage exit). Resource-dominant provinces: d ≈ 0.65 (asymmetric—benefit from equalization but subordinated on resource control; constrained exit with some renegotiation capacity). Central Canadian provinces: d ≈ 0.25 (beneficiary with modest costs—net transfer recipients, market protection; mobile exit because they benefit from the arrangement). Indigenous nations: d ≈ 0.92 (near full target—identity-locked, excluded from the frame entirely, sovereignty claim negated by both federal and provincial law). Judiciary: d ≈ 0.5 (analytical—observes and enforces but does not collect or pay; sits at the midpoint structurally).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT a mandatrophy case. The founding problem (creating a unified nation-state from diverse colonies with conflicting interests) remains live and contested. The constitutional subordination reading answers it by asserting federal supremacy. The rising suppression and theater metrics over the interval indicate the constraint is under increasing political pressure, not atrophy—the coordinate function is challenged, not forgotten. Provinces contest the reading's legitimacy but accept its legal force, generating active resistance rather than inertial persistence. If the founding problem were dead (national unity no longer valued, provinces universally accepted subordination), the theater_ratio would rise toward 0.7+ and suppression would fall as enforcement became purely performative. Neither is happening: suppression is rising because provinces actively challenge the constraint, and theater is moderate because both coordination and extraction remain structurally operative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_text_indeterminacy,
    'Does the Constitution Act 1867 textually entail federal supremacy and provincial subordination, or is subordination a reading imposed upon an ambiguous text?',
    'Historical textual analysis and originalist vs. living-constitution comparison: do the drafters'' own statements support supremacy, or did they leave the relation ambiguous? Do other jurisdictions with similar texts (Australia, US) interpret them as subordinating or as confederal compacts? What do the Confederation debates and the Quebec Resolutions (1864) actually assert about provincial sovereignty?',
    'If textual reading supports subordination, the constraint is constitutionally grounded and the sibling readings (compact, resource sovereignty) are non-canonical. If the text is indeterminate, all three readings are equipoise and the contest is purely political, not constitutional—the subordination reading prevails by power, not by law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_text_indeterminacy, empirical, 'Whether constitutional subordination is entailed by the constitutional text or is a contestable reading.').

omega_variable(
    kernel_contestation_vs_settlement,
    'Is this kernel genuinely contested or has the Supreme Court''s interpretation settled it into law?',
    'Track the political challenge to the court''s rulings (Reference re Secession of Quebec was affirmed in 1998; has political acceptance of that ruling grown or eroded?). If provinces continue to claim sovereignty and exit rights despite the ruling, the kernel remains contested. If the ruling becomes internalized as settled constitutional law, the constraint transitions from contested to fixed—the suppression requirement would decline as the constraint becomes normalized.',
    'A truly settled constraint would show falling resistance and suppression as it becomes internalized norm. Continued high resistance and rising suppression indicate the constraint is performatively enforced, not settled—it persists as law but not as consensus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contestation_vs_settlement, empirical, 'Whether judicial settlement has truly resolved the constitutional contest or whether the political dispute persists below the legal surface.').

omega_variable(
    resource_sovereignty_structural_separability,
    'Is section 92A (provincial ownership of natural resources) structurally reconcilable with constitutional subordination, or does resource ownership ground a sovereignty claim that contradicts subordination?',
    'Test cases in which resource-dominant provinces assert s.92A rights against federal climate policy or interprovincial trade rules. If courts uphold federal override, resource ownership is compatible with subordination. If courts recognize resource ownership as a competing sovereignty locus, the constraint (subordination) is partially foreclosed by the resource sovereignty reading.',
    'If resource ownership is separable from sovereignty (courts hold it is), the subordination reading is stable. If resource ownership grounds sovereignty claims (courts hold it is), the constraint bifurcates: subordination holds in some domains, resource sovereignty in others. The constraint would require reclassification or decomposition into separate stories per domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_sovereignty_structural_separability, empirical, 'Whether provincial resource ownership can coexist with constitutional subordination or whether it grounds a competing sovereignty claim.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression structural (constitutional law, federal legal veto) or internalized (provinces accept subordination as legitimate), or both? What fraction is each?',
    'Post-constraint-removal trajectory: if a province obtained independence or a province''s exit became legal, would the province remain subordinated by belief/culture/identity, or would the suppression dissolve? Track separatist sentiment in provinces with higher vs. lower federal transfers; if redistribution reduces separatism, suppression is partly internalized (identity with the union through fiscal benefit). If separatism persists despite transfers, suppression is structural (legal constraint, not cultural).',
    'If suppression is structural, the constraint''s effective suppression is purely the legal mechanism (constitutional law). If internalized, the target carries the suppression with them even if the legal mechanism were removed—the constraint would persist through cognitive capture. Internalization increases effective suppression beyond the structural measure; it is the mechanism by which a tangled_rope becomes more snare-like without metric change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural (constitutional law, exit veto) or internalized (cultural/identity acceptance of subordination).').

omega_variable(
    indigenous_sovereignty_kernel_contradiction,
    'Can indigenous nations'' pre-constitutional sovereignty claims be reconciled with either provincial subordination or provincial sovereignty, or do they form an incommensurable third pole that dissolves the provincial-federal dyad?',
    'Tracking indigenous jurisprudence and co-jurisdiction agreements: do courts recognize indigenous sovereignty as a third node (federal, provincial, indigenous)? Or do they subsume indigenous claims under federal or provincial authority? The residential schools reckoning (2021+) and ongoing TRC implementation suggest courts are moving toward three-node recognition. If so, the provincial-federal constraint becomes partial—it governs provincial-federal relations but not the three-node space.',
    'If indigenous sovereignty becomes formally recognized and co-jurisdictional, the provincial-federal dyad is no longer the complete frame. The constraint (subordination) would be reclassified as partial or local (federal-provincial only) rather than binding on the full territorial space. This would require decomposition into separate constraints per dyad (federal-provincial, federal-indigenous, provincial-indigenous), each with different ε and different beneficiary/victim structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_sovereignty_kernel_contradiction, conceptual, 'Whether indigenous sovereignty can be integrated into the provincial-federal frame or whether it requires a fundamentally different constraint structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__constitutional_subordination, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t0, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(prov_tr_t0, observed).
narrative_ontology:measurement(prov_tr_t8, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(prov_tr_t8, observed).
narrative_ontology:measurement(prov_tr_t16, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(prov_tr_t16, observed).
narrative_ontology:measurement(prov_tr_t25, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(prov_tr_t25, observed).
narrative_ontology:measurement(prov_tr_t35, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(prov_tr_t35, observed).
narrative_ontology:measurement(prov_tr_t50, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(prov_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(prov_be_t0, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(prov_be_t0, observed).
narrative_ontology:measurement(prov_be_t8, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(prov_be_t8, observed).
narrative_ontology:measurement(prov_be_t16, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 16, 0.58).
narrative_ontology:measurement_basis(prov_be_t16, observed).
narrative_ontology:measurement(prov_be_t25, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 25, 0.65).
narrative_ontology:measurement_basis(prov_be_t25, observed).
narrative_ontology:measurement(prov_be_t35, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 35, 0.67).
narrative_ontology:measurement_basis(prov_be_t35, observed).
narrative_ontology:measurement(prov_be_t50, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(prov_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t0, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(prov_su_t0, observed).
narrative_ontology:measurement(prov_su_t8, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 8, 0.6).
narrative_ontology:measurement_basis(prov_su_t8, observed).
narrative_ontology:measurement(prov_su_t16, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 16, 0.65).
narrative_ontology:measurement_basis(prov_su_t16, observed).
narrative_ontology:measurement(prov_su_t25, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(prov_su_t25, observed).
narrative_ontology:measurement(prov_su_t35, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(prov_su_t35, observed).
narrative_ontology:measurement(prov_su_t50, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(prov_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__constitutional_subordination, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(provincial_sovereignty_boundary__constitutional_subordination, 0.12).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__compact_federalism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__resource_sovereignty_primacy).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, federal_spending_power_legitimacy).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, equalization_transfer_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, canadian_climate_federalism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the provincial_sovereignty_boundary kernel. The constitutional_subordination reading (this file) asserts federal supremacy and provincial subordination; it is contested by compact_federalism (provinces are sovereign parties to a negotiated compact) and resource_sovereignty_primacy (provincial resource ownership grounds absolute territorial sovereignty). These are not perspective variants of the same constraint—they have different ε values, different beneficiary/victim structures, and different types. All three are live readings held by different political actors. They affect each other: subordination undermines compact federalism; resource sovereignty challenges subordination; compact federalism would foreclose subordination if politically ascendant. Each reading must be authored as a separate constraint story with its own ε-invariant classification. The network links show the contamination pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(provincial_sovereignty_boundary__constitutional_subordination, institutional, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
