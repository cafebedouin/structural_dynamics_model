% ============================================================================
% CONSTRAINT STORY: federation_membership__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__integration_reading, []).

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
 *   constraint_id: federation_membership__integration_reading
 *   human_readable: Federation Membership as Irreversible Integration with Constitutional Free Movement
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   A continental federation whose treaties grant every citizen an
 *   individually enforceable right to live and work anywhere in the union,
 *   administered by supranational institutions that treat the treaties as a
 *   constitutional charter and membership as irreversible. Under this
 *   reading, national border control over fellow union citizens is not a
 *   policy option but a legal impossibility, and national measures
 *   conflicting with movement law are struck down. This is the
 *   integration_reading of the federation_membership kernel: the same
 *   institutional complex admits a sovereignty_reading (membership as
 *   conditional treaty; borders legitimate; movement negotiable) with a
 *   different ε and a different victim set, so the two are authored as
 *   separate ε-invariant stories and linked via network.affects_constraints.
 *   ε here is high relative to the sibling not because movement itself is
 *   harmful but because this reading's core premise — border restriction
 *   illegitimate — removes the victim set's remedy and constitutionalizes the
 *   transfer.
 *
 * KEY AGENTS:
 *   - supranational_governance_institutions: Agenda setter (institutional/arbitrage) — drafts and enforces the movement acquis; constitutionalizes the right through adjudication
 *   - mobile_citizens: Primary beneficiary (moderate/arbitrage) — holds the portable movement right; captures wage and opportunity differentials directly
 *   - employers_of_mobile_labor: Beneficiary and receipt seat (powerful/arbitrage) — gains staffing elasticity and wage moderation from the continental labor pool
 *   - local_labor_markets: Primary target (powerless/trapped) — place-bound workers in receiving regions absorb displacement; the border remedy is constitutionally disabled
 *   - emigration_region_communities: Secondary target (powerless/trapped) — sending regions lose their young workforce and tax base with no offsetting lever
 *   - member_state_governments: Dual seat (institutional/constrained) — co-legislator yet unable to unilaterally restrict inflows; absorbs blame for outcomes set above it
 *   - national_electorates: Excluded (organized/constrained) — prefer restriction, but the right sits above their electoral lever
 *   - comparative_political_economists: Analytical observer (analytical/analytical) — sees the full transfer structure across federations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__integration_reading, 0.68).
domain_priors:suppression_score(federation_membership__integration_reading, 0.62).
domain_priors:theater_ratio(federation_membership__integration_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership__integration_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(federation_membership__integration_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership__integration_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__integration_reading, "Federation Membership as Irreversible Integration with Constitutional Free Movement").
narrative_ontology:topic_domain(federation_membership__integration_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__integration_reading, '6f7589e9-97c1-4cc0-8ef7-107d1f9689db').
narrative_ontology:cs_kernel_codification('6f7589e9-97c1-4cc0-8ef7-107d1f9689db', fixed_text).
narrative_ontology:cs_authority_grounding('6f7589e9-97c1-4cc0-8ef7-107d1f9689db', lineage).
narrative_ontology:cs_interpretation_layer_present('6f7589e9-97c1-4cc0-8ef7-107d1f9689db').
narrative_ontology:cs_reading_relation('6f7589e9-97c1-4cc0-8ef7-107d1f9689db', federation_membership__sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('6f7589e9-97c1-4cc0-8ef7-107d1f9689db', foundational, free_movement_as_constitutional_right).
narrative_ontology:cs_axiom_status(free_movement_as_constitutional_right, holdable).
narrative_ontology:cs_axiom_grounding('6f7589e9-97c1-4cc0-8ef7-107d1f9689db', free_movement_as_constitutional_right, conventional).
narrative_ontology:cs_axiom('6f7589e9-97c1-4cc0-8ef7-107d1f9689db', foundational, membership_irreversibility).
narrative_ontology:cs_axiom_status(membership_irreversibility, holdable).
narrative_ontology:cs_axiom_grounding('6f7589e9-97c1-4cc0-8ef7-107d1f9689db', membership_irreversibility, conventional).
narrative_ontology:cs_reference_frame('6f7589e9-97c1-4cc0-8ef7-107d1f9689db', ever_closer_union_order).
narrative_ontology:cs_drift_state('6f7589e9-97c1-4cc0-8ef7-107d1f9689db', contemporary, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6f7589e9-97c1-4cc0-8ef7-107d1f9689db', '2026-06-21T00:00:00Z').
narrative_ontology:cs_kernel_id(federation_membership__integration_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, mobile_citizens).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, employers_of_mobile_labor).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, supranational_governance_institutions).
narrative_ontology:constraint_victim(federation_membership__integration_reading, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership__integration_reading, emigration_region_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership__integration_reading, member_state_governments).
narrative_ontology:constraint_vindicates(federation_membership__integration_reading, free_movement_constitutionalism_doctrine).
narrative_ontology:constraint_vindicates(federation_membership__integration_reading, ever_closer_union_telos).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The union's commission, court, and parliament: they draft the movement rules, adjudicate conflicts between the rules and national measures, and treat the treaties as a constitutional charter rather than an ordinary treaty. They collect budget contributions and, more consequentially, decision authority: each enlargement round and each adjudicated dispute extends the competence they administer. Their stated justification is completing the single market and securing peace through integration.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, supranational_governance_institutions, agenda_setter,
    institutional, generational, arbitrage, continental).

% Workers, students, and retirees who use the movement right. They hold an individually enforceable entitlement to reside and work anywhere in the union, with portable benefits and recognition frameworks; they capture the wage and opportunity differential directly, and their answer to a bad local labor market is to leave it for a better one.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, mobile_citizens, beneficiary,
    moderate, biographical, arbitrage, continental).

% Firms that recruit across the union. A continental labor pool gives them staffing elasticity in tight sectors and moderates wage growth where inflows are strong; posting and relocation rules let them site operations where costs are lowest. They lobby for keeping movement rules uniform and oppose national derogations.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, employers_of_mobile_labor, beneficiary,
    powerful, generational, arbitrage, continental).

% Place-bound workers, households, and regional wage-and-bargaining structures in receiving areas. They absorb wage pressure and job competition in exposed sectors when mobility inflows rise; their assets — homes, community ties, occupation- and region-specific skills — do not move with them, so relocating means writing those assets down. The lever their national politics would once have supplied for managing inflow pace no longer exists at the national level; union-level adjustment funds arrive slowly and diffusely relative to the shock.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, local_labor_markets, payer,
    powerless, biographical, trapped, regional).

% Sending regions, mostly at the union's periphery. Their young and credentialed workers leave for the core; the remaining population is older, the tax base thinner, and public services harder to sustain. The community as such cannot move, and the right that lets its members leave provides the region no offsetting lever; structural funds partially compensate but have not reversed the flow.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, emigration_region_communities, payer,
    powerless, generational, trapped, regional).

% They sit in the union's council and co-write the rules, and they administer residence and labor-market policy domestically — but they cannot unilaterally limit inflows of fellow union citizens even when their voters demand it; defections meet infringement proceedings and treaty-obligation enforcement. Leaving the union is formally available but priced so severely that, under this reading, it is not treated as a working option; they absorb domestic blame for outcomes set above them.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, member_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership__integration_reading, member_state_governments, agenda_setter).

% Majorities in several member states periodically vote for tighter movement rules. Changing the national government does not change the movement right, which sits in the treaty layer above national legislation; referenda on union membership or movement terms have been treated as union-level negotiating events rather than as binding instructions to change the rule itself.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, national_electorates, excluded,
    organized, generational, constrained, national).

% Scholars of federalism, migration, and labor markets. They measure displacement elasticities, enforcement patterns, and lock-in effects across federations and publish the comparisons; they neither collect from the arrangement nor pay into it, though their research agendas follow its controversies.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, comparative_political_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__integration_reading, employers_of_mobile_labor).
narrative_ontology:fixing_cost_class(federation_membership__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves continent-scale labor allocation and market integration once, centrally: a single movement right plus mutual recognition replaces a web of bilateral migration and residence agreements, lets labor flow toward demand, and underwrites the single market and shared currency area. It also solved the founding credibility problem: binding national economies and populations tightly enough that war among members became materially self-defeating.
% TRANSFER_FUNCTION: Moves staffing elasticity and wage moderation to employers, wage-and-opportunity differentials to mobile citizens, and decision authority (plus budget contributions) to supranational institutions; moves displacement and wage-pressure costs onto place-bound workers in receiving regions, workforce loss onto sending regions, and border authority away from member-state governments — with adjustment funds moving a small, slow compensating flow back toward the regions bearing the costs.
% ABSENT_VOICES: National electorates that prefer tighter movement rules: they can change governments but not the treaty-layer right, and referenda results have been negotiated around rather than implemented. Immobile workers in exposed sectors are formally enfranchised, but the operative lever they would pull — national migration policy — has been removed from the national menu, so their objection arrives at a level that no longer holds the switch.
% DISAPPEARANCE_RATIONALE: If the constitutional movement right vanished overnight: tens of millions of cross-border residents' statuses would require renegotiation, labor markets would re-nationalize, sectors staffed by mobile labor would face immediate shortages, sending and receiving regions would reprice their wage and fiscal structures, and member-state relations would reopen treaty by treaty — the continental economy's organizing principle would be gone, not merely amended.
% FOUNDING_PROBLEM: Post-war reconciliation and reconstruction: bind the continent's economies and peoples so that war among member states becomes materially impossible, and match abundant labor in some regions to reconstruction demand in others without beggar-thy-neighbor border regimes.
% FOUNDING_PROBLEM_CORROBORATION: The reconciliation rationale is attested outside the benefiting parties: founding-era treaty preambles, accession-era national parliamentary records, and independent historiography of the integration project all document war-prevention as the founding purpose. Benefiting parties (supranational institutions, mobile-citizen constituencies) attest a continuing market-completion rationale; sovereignty parties and several member-state electorates attest the founding problem is solved and no longer justifies the lock-in — hence contested.
narrative_ontology:disappearance_verdict(federation_membership__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__integration_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-18',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k3', 'max_tokens=32000,temperature=default,reasoning=max').
narrative_ontology:story_seed(federation_membership__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__integration_reading, 0.68, 'kimi-k3', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.68 at interval end: the transfer is real and concentrated — wage pressure and displacement in exposed receiving-region sectors, workforce loss in sending regions — while the coordination benefit is broad but diffuse; successive enlargement rounds widened the wage differentials the arrangement arbitrages, which is why the series climbs from 0.42. Suppression 0.62: not physical coercion but constitutional disablement — the remedy (national border control) is removed from the policy menu and defection meets infringement machinery; suppression is authored as a raw structural property, unscaled by power or scope, and its rising series tracks the progressive constitutionalization of the right (direct effect, primacy, enlargement of the acquis) rather than episodic enforcement. Theater 0.35: the consultation and adjustment-fund apparatus is partly performative — funds are real but small relative to shocks, and 'managed mobility' rhetoric recurs without derogations — while the core adjudication and enforcement is functional. Accessibility_collapse 0.60: inside the union, alternatives to the single movement regime (national quotas, bilateral deals) are substantially collapsed; outside it, states run their own regimes, so collapse is not total. Resistance 0.62: persistent organized resistance — referenda, sovereignty parties, crisis-driven border reimpositions — that the arrangement meets and overrides rather than absorbs. gain_flow is authored to employers_of_mobile_labor: the displacement cost borne by local labor markets has as its counterpart a concrete wage-moderation and elasticity gain captured in employer margins; supranational institutions accrue authority, which is real, but authority is not the extraction receipt. fixing_cost is prohibitive: treaty revision requires unanimity across member states and the interpretive layer treats the right as indivisible, so the seats that could fix it face costs far above the benefit of fixing.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes per-seat classifications from the structural data; the expected divergence: from the mobile_citizens and employers seats, effective extraction is low or inverted — the arrangement subsidizes them with option value and labor supply. From local_labor_markets and emigration_region_communities — trapped, high directionality — the same treaty structure computes as substantially extractive. member_state_governments sit between: they hold voice without unilateral control. national_electorates' seat is defined by removal from the operative decision. The gap is not a disagreement about facts; it is one treaty structure read from different exit positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (mobile_citizens with arbitrage-grade exit, employers, supranational institutions) derive low d — the constraint subsidizes them. Trapped victims (local_labor_markets, emigration_region_communities) derive high d, near the full-target end, because their assets are place-bound and the remedy level has been moved above their reach. member_state_governments are declared in neither array: their intermediate position (co-legislative voice, no unilateral control, prohibitive-priced exit) falls to the institutional fallback, which is descriptively adequate — no directionality override is authored, because an override keys on power_atom and would wrongly move the supranational institutions that share the institutional atom. national_electorates are excluded; their structural fact is not a d value but the relocation of the decision above their lever.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding reconciliation problem (war among members) is substantially dead; the labor-allocation problem remains live. The classification prevents two mislabelings: romanticizing the arrangement as pure coordination (the movement right is genuinely valued by millions, but its constitutionalization suppresses the payers' remedy — that is the tangled structure, not a clean coordination win) and demonizing it as pure predation (the coordination function is not cover; single-market integration and the portable right are real goods). The mandatrophy risk lives in the drift of justification from reconciliation instrument to market-completion instrument while the lock-in persisted unchanged. founding_problem_status is authored contested and disappearance_verdict is world_rearranges, so no zombie mismatch fires, but the genealogy shift is documented for the mismatch consumer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is federation membership a constitutional, irreversible integration (this reading) or a conditional treaty under which national border legitimacy survives (sovereignty_reading)?',
    'Authoritative handling of withdrawal and border-reimposition episodes: if a member state reimposes controls or exits and renegotiates movement obligations as ordinary treaty business, the conditional-treaty reading gains structural support; if such moves are treated as legal impossibilities requiring union-level authorization, this reading is confirmed.',
    'If the sovereignty_reading prevails, the victim and excluded structure inverts: member-state electorates become legitimate principals rather than absent voices, local labor markets gain a lawful remedy, and the ε attributable to suppressed border policy drops accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'This constraint is one reading of the federation_membership kernel; the sibling sovereignty_reading inverts the border-legitimacy premise, and the disagreement is located in the irreversibility and border-legitimacy premises.').

omega_variable(
    labor_displacement_magnitude,
    'How large and persistent is the wage and employment displacement actually borne by local labor markets in receiving regions?',
    'Meta-analysis of migration-wage elasticity studies across member states, disaggregated by sector and skill level, plus longitudinal regional adjustment studies of post-enlargement mobility shocks.',
    'If displacement is small and transitory, base ε falls toward the coordination floor and the story reclassifies toward a coordination type; if large and concentrated, the tangled structure is confirmed and the adequacy of compensatory mechanisms becomes the operative policy question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_displacement_magnitude, empirical, 'Empirical magnitude of labor-market displacement, which sets how much of the measured extraction is real transfer versus coordination cost.').

omega_variable(
    irreversibility_fact_status,
    'Is the claimed irreversibility of membership an empirical fact about the structure, or a normative posture maintained by pricing exit catastrophically?',
    'Track completed withdrawal and opt-out episodes: whether exiting states renegotiate movement obligations as ordinary treaty matters at tolerable cost (evidence of conditionality) or face cost structures that make exit effectively prohibitive (evidence of constructed irreversibility).',
    'If withdrawal proves practically available at tolerable cost, member_state_governments'' exit_options improve, their effective extraction falls, and the suppression metric must be revised downward; if exit is prohibitive by construction, the suppression reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_fact_status, empirical, 'Whether membership irreversibility is a structural fact or an enforcement posture.').

omega_variable(
    kernel_locus_ambiguity,
    'Is the kernel the treaty text as ratified (amendable by unanimity) or the constitutional practice built on it by supranational adjudication (amendable only by adjudicative retreat)?',
    'Doctrinal analysis of how conflicts between treaty text and accumulated case law are resolved, and observation of whether treaty revision or judicial reinterpretation is the operative amendment channel in practice.',
    'If the kernel is the text, unanimous treaty revision is a live fix and fixing_cost may drop from prohibitive; if the kernel is adjudicative practice, the interpretation layer absorbs revision pressure and the constraint is stickier than the text suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_locus_ambiguity, conceptual, 'Framing under-determination: the kernel may be the fixed text or the constitutional practice layered above it; the choice changes how fixable the arrangement is.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__integration_reading, 0, 65).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmi_tr_t0, federation_membership__integration_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(fmi_tr_t13, federation_membership__integration_reading, theater_ratio, 13, 0.2).
narrative_ontology:measurement(fmi_tr_t26, federation_membership__integration_reading, theater_ratio, 26, 0.24).
narrative_ontology:measurement(fmi_tr_t39, federation_membership__integration_reading, theater_ratio, 39, 0.28).
narrative_ontology:measurement(fmi_tr_t52, federation_membership__integration_reading, theater_ratio, 52, 0.32).
narrative_ontology:measurement(fmi_tr_t65, federation_membership__integration_reading, theater_ratio, 65, 0.35).

% Extraction over time
narrative_ontology:measurement(fmi_be_t0, federation_membership__integration_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fmi_be_t13, federation_membership__integration_reading, base_extractiveness, 13, 0.47).
narrative_ontology:measurement(fmi_be_t26, federation_membership__integration_reading, base_extractiveness, 26, 0.52).
narrative_ontology:measurement(fmi_be_t39, federation_membership__integration_reading, base_extractiveness, 39, 0.57).
narrative_ontology:measurement(fmi_be_t52, federation_membership__integration_reading, base_extractiveness, 52, 0.63).
narrative_ontology:measurement(fmi_be_t65, federation_membership__integration_reading, base_extractiveness, 65, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fmi_su_t0, federation_membership__integration_reading, suppression_requirement, 0, 0.36).
narrative_ontology:measurement(fmi_su_t13, federation_membership__integration_reading, suppression_requirement, 13, 0.42).
narrative_ontology:measurement(fmi_su_t26, federation_membership__integration_reading, suppression_requirement, 26, 0.48).
narrative_ontology:measurement(fmi_su_t39, federation_membership__integration_reading, suppression_requirement, 39, 0.54).
narrative_ontology:measurement(fmi_su_t52, federation_membership__integration_reading, suppression_requirement, 52, 0.58).
narrative_ontology:measurement(fmi_su_t65, federation_membership__integration_reading, suppression_requirement, 65, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__integration_reading, resource_allocation).
narrative_ontology:affects_constraint(federation_membership__integration_reading, federation_membership__sovereignty_reading).

% DUAL FORMULATION NOTE:
% Constraint family: two readings of the federation_membership kernel. This story (integration_reading) holds membership irreversible and free movement constitutional; its ε is high because the border remedy is constitutionally disabled and displacement costs fall on local labor markets and sending regions. The sibling sovereignty_reading holds membership conditional and borders legitimate; under it the victim/excluded structure inverts and ε differs by a wide margin. The readings are not one constraint from two angles — they instantiate different constraints from the same institutional complex, so they are decomposed per ε-invariance and linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
