% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__constitutional_impossibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__constitutional_impossibility_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: secession_legitimacy_boundary__constitutional_impossibility_reading
 *   human_readable: Union Permanence Doctrine — Unilateral Secession Impermissible, Amendment-Negotiated Exit Only
 *   domain: political economy/federalism/resource politics
 *
 * SUMMARY:
 *   A federal constitutional order bars unilateral provincial secession and
 *   channels all legitimate exit through a supermajority amendment process
 *   requiring concurrence of both the exiting unit and the remainder of the
 *   federation. This file instantiates the constitutional-impossibility
 *   reading of the secession_legitimacy_boundary kernel: it treats permanence
 *   as constitutive constitutional law rather than policy, holds separatist
 *   legitimacy claims categorically void, and consequently authors an EMPTY
 *   victim set — the reading denies the extraction characterization outright,
 *   so no agent enters base_properties.victims. Its epsilon is indexed to
 *   that stance (OQ-26): 0.38 over the standing arrangement as this reading
 *   assesses it, acknowledging heavy but deemed-legitimate membership
 *   obligations on higher-revenue provinces while rejecting the claim that
 *   these justify exit outside the amendment channel. The three sibling
 *   readings — popular sovereignty, grievance threshold, treaty primacy — are
 *   separate files with their own beneficiary/victim structures; per the
 *   kernel rules their contents are routed to omega variables here and are
 *   not folded into this constraint. The claim/metric gap is deliberate and
 *   is the datum: the reading CLAIMS constitutive permanence (mountain,
 *   emergent-natural framing), while the authored metrics describe an
 *   actively enforced arrangement with a never-used exit door, rising
 *   enforcement investment, and accumulating cost concentration. That
 *   divergence is what the engine measures; nothing here reconciles it. KEY
 *   AGENTS (by structural relationship): - federal_government: agenda-setter
 *   (institutional/arbitrage) — administers the permanence doctrine, sets
 *   negotiation conditions, collects the unified fiscal base -
 *   constitutional_judiciary: agenda-setter/enforcer
 *   (institutional/constrained) — certifies questions, refuses effect to
 *   out-of-channel exit attempts - recipient_provinces: primary beneficiary
 *   (organized/constrained) — accrue transfers secured by locked-in
 *   membership - rest_of_federation_electorates: beneficiary veto-holder
 *   (organized/constrained) — ratification gate on any exit -
 *   resource_rich_net_contributor_province: principal cost-bearer
 *   (powerful/constrained; secondarily positioned as beneficiary via
 *   common-market and insurance access) - provincial_separatist_movements:
 *   direct target of the categorical prohibition (organized/identity_locked)
 *   - linguistic_minorities_in_contested_regions: cost-bearer under either
 *   outcome (moderate/constrained) - indigenous_treaty_holders: excluded seat
 *   — lands implicated by any boundary change, unseated from amendment tables
 *   (organized/trapped) - comparative_constitutional_scholarship: analytical
 *   observer — documents that no amendment-route exit has ever completed
 *
 * KEY AGENTS:
 *   - federal_government: agenda-setter (institutional/arbitrage) — administers the permanence doctrine, sets the timing and conditions of any negotiation table, decides which referendum questions count as clear; draws revenue depth, defense integration, and diplomatic standing from undivided territory
 *   - constitutional_judiciary: enforcing agenda-setter (institutional/constrained) — certifies which secession questions proceed and refuses effect to exit attempts outside the amendment channel; precedent-bound
 *   - recipient_provinces: primary beneficiary (organized/constrained) — receive equalization and program transfers whose scale presupposes contributor provinces cannot walk away
 *   - rest_of_federation_electorates: beneficiary veto-holder (organized/constrained) — ratify or block any exit amendment; benefit from intact internal market and pooled debt
 *   - resource_rich_net_contributor_province: principal cost-bearer (powerful/constrained, secondary beneficiary) — largest net fiscal contributor; formal exit requires simultaneous dual supermajorities never yet assembled
 *   - provincial_separatist_movements: direct target of the prohibition (organized/identity_locked) — organizational existence constituted by pursuit of the blocked objective
 *   - linguistic_minorities_in_contested_regions: cost-bearer under either outcome (moderate/constrained) — exposed to relocation costs and language-regime shifts each crisis cycle
 *   - indigenous_treaty_holders: excluded (organized/trapped) — hold pre-constitutional treaties straddling any prospective boundary; absent from ratification formulas
 *   - comparative_constitutional_scholarship: analytical observer (analytical/global) — documents the worldwide record that no pre-existing amendment route to secession has ever been completed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.38).
domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.68).
domain_priors:theater_ratio(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__constitutional_impossibility_reading, mountain).
narrative_ontology:human_readable(secession_legitimacy_boundary__constitutional_impossibility_reading, "Union Permanence Doctrine — Unilateral Secession Impermissible, Amendment-Negotiated Exit Only").
narrative_ontology:topic_domain(secession_legitimacy_boundary__constitutional_impossibility_reading, "political economy/federalism/resource politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__constitutional_impossibility_reading).
domain_priors:emerges_naturally(secession_legitimacy_boundary__constitutional_impossibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__constitutional_impossibility_reading, '4cec5cde-cfae-47f7-8515-1d14156b7512').
narrative_ontology:cs_kernel_codification('4cec5cde-cfae-47f7-8515-1d14156b7512', fixed_text).
narrative_ontology:cs_authority_grounding('4cec5cde-cfae-47f7-8515-1d14156b7512', lineage).
narrative_ontology:cs_interpretation_layer_present('4cec5cde-cfae-47f7-8515-1d14156b7512').
narrative_ontology:cs_reading_relation('4cec5cde-cfae-47f7-8515-1d14156b7512', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('4cec5cde-cfae-47f7-8515-1d14156b7512', secession_legitimacy_boundary__grievance_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('4cec5cde-cfae-47f7-8515-1d14156b7512', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('4cec5cde-cfae-47f7-8515-1d14156b7512', foundational, constitutional_amendment_exclusive_exit).
narrative_ontology:cs_axiom_status(constitutional_amendment_exclusive_exit, holdable).
narrative_ontology:cs_axiom_grounding('4cec5cde-cfae-47f7-8515-1d14156b7512', constitutional_amendment_exclusive_exit, conventional).
narrative_ontology:cs_axiom('4cec5cde-cfae-47f7-8515-1d14156b7512', foundational, permanence_preconditions_federation_goods).
narrative_ontology:cs_axiom_status(permanence_preconditions_federation_goods, holdable).
narrative_ontology:cs_axiom_grounding('4cec5cde-cfae-47f7-8515-1d14156b7512', permanence_preconditions_federation_goods, instrumental).
narrative_ontology:cs_reference_frame('4cec5cde-cfae-47f7-8515-1d14156b7512', indestructible_union_doctrine).
narrative_ontology:cs_drift_state('4cec5cde-cfae-47f7-8515-1d14156b7512', contemporary_post_referendum_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('4cec5cde-cfae-47f7-8515-1d14156b7512', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, recipient_provinces).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, rest_of_federation_electorates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, resource_rich_net_contributor_province).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, resource_rich_net_contributor_province).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, provincial_separatist_movements).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, linguistic_minorities_in_contested_regions).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, territorial_integrity_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, amendment_exclusivity_principle).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, clarity_requirement_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the constitutional order in which provincial exit requires a negotiated amendment ratified by supermajorities across the federation. Sets the timing, conditions, and framing of any negotiation table, and determines which referendum questions count as sufficiently clear to engage it. Draws continuing revenue depth, defense integration, and diplomatic standing from undivided territory. It can reshape the rules it enforces at lower cost than any other seat, since any redesign still runs through institutions it dominates.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Certifies which secession-related questions may proceed and refuses effect to exit attempts made outside the amendment channel. Its rulings supply the operative meaning of the permanence principle, including conditions added after referenda about when negotiation must follow a clear vote. Appointment structures and precedent bind it; revisiting the doctrine would require roughly the same cross-jurisdictional consensus the doctrine itself demands of everyone else.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Receive ongoing equalization and program transfers whose scale presupposes that higher-revenue provinces remain members. Individually unable to alter the amendment formula; collectively able to swing ratification. Their budgets treat the inflows as baseline, and leaving the pool would mean surrendering receipts they have built spending around. They defend the permanence framework in intergovernmental forums as a matter of fiscal continuity.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, recipient_provinces, beneficiary,
    organized, biographical, constrained, regional).

% Hold ratification power over any exit amendment and draw benefit from the intact internal market, pooled debt service, and shared defense the union finances. They experience the permanence rule as background constitutional furniture and mobilize around it mainly in the months when a referendum forces the question onto their ballots.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, rest_of_federation_electorates, beneficiary,
    organized, biographical, constrained, national).

% Contributes the largest net fiscal outflows in the federation while drawing common-market access, currency stability, defense provision, and countercyclical support in downturn years. Its formal path out runs through an amendment requiring simultaneous supermajorities in its own legislature and across the rest of the federation — a combination that has never been assembled anywhere. Commodity wealth buys it bargaining leverage inside the rules, not a route around them; its grievances are aired in intergovernmental conferences whose agendas the federal order chairs.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, resource_rich_net_contributor_province, payer,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__constitutional_impossibility_reading, resource_rich_net_contributor_province, beneficiary).

% Contest elections and run referenda on leaving the federation; the categorical bar on unilateral exit defines the exact boundary their project presses against. Cadre careers, donor coalitions, and organizational infrastructure are all built around pursuing the blocked objective, so standing down would dissolve the movement rather than relieve it. Each referendum cycle regenerates the organization even when the vote fails.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, provincial_separatist_movements, payer,
    organized, generational, identity_locked, regional).

% Live inside jurisdictions whose borders are the subject of the dispute and would hold minority status under either outcome. They absorb relocation costs, property-value exposure, and language-regime uncertainty during each crisis cycle. Their recourse is rights litigation and gradual internal migration toward other provinces — both slow and costly, and neither gives them a seat in the ratification formulas.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, linguistic_minorities_in_contested_regions, payer,
    moderate, biographical, constrained, regional).

% Hold treaties predating both orders of government whose territories straddle the boundaries any exit would redraw. They are not constituents of the federal-provincial amendment tables and do not appear in the ratification formulas; consultation happens informally, after negotiating positions have hardened, or not at all. Unlike every other seat, they cannot relocate out of the question — their relationship to the land is the stake.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, indigenous_treaty_holders, excluded,
    organized, civilizational, trapped, continental).

% Tracks how federations worldwide handle member exit, compiles the record showing that no federation has completed secession through a pre-existing amendment route, and supplies the outside assessment of whether permanence principles track structural necessity or particular design choices. Collects nothing and pays nothing; its standing comes from being outside every national ratification formula.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, comparative_constitutional_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__constitutional_impossibility_reading, recipient_provinces).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__constitutional_impossibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Makes multi-decade federation commitments credible: pooled defense financing, a single internal market, shared currency and debt service, and countercyclical transfers across regions all presuppose that membership cannot be revoked unilaterally after an adverse shock. The impermissibility rule removes the walk-away option that would otherwise price every transfer as hostage-taking and every long-lived joint investment as unrecoverable.
% TRANSFER_FUNCTION: Moves recurring net fiscal transfers from higher-revenue provinces into the federation pool and onward to recipient provinces; and moves authority over territorial-membership questions from provincial majorities to a joint supermajority amendment process in which the exiting unit and the remainder must concur.
% ABSENT_VOICES: Indigenous treaty holders would object that consent formulas ignore prior occupancy and that their lands straddle every prospective boundary — they are unseated from the amendment tables (authored above as the excluded seat). Linguistic minorities in contested regions hold stakes in both outcomes yet enter the process only through litigation after positions have hardened. Neither seat appears in any ratification formula; dissent is priced out of the room rather than voiced within it.
% DISAPPEARANCE_RATIONALE: Overnight repeal would reopen the walk-away option: bond markets would reprice provincial debt against fragmentation risk, transfer pools would unwind as contributors renegotiated or exited, defense and trade arrangements would fragment along new borders, and every future asymmetric shock would restart dissolution bargaining from scratch. Removal of the arrangement rearranges the fiscal, security, and market architecture immediately — nothing about the current equilibrium survives its absence.
% FOUNDING_PROBLEM: Founding-era fragility: loose confederations were dissolving or paralyzed — unable to finance common defense, service shared revolutionary debts, or hold a customs area — because any member could withdraw or hold out. Durable union required closing the unilateral withdrawal option before credit markets and defense planners would underwrite the joint enterprise.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: comparative-federalism scholarship and archival founding correspondence (defense-financing and debt-service motives) attest that the founding fragility problem was real, and the record of collapsed nineteenth-century confederations is independent of any current beneficiary's account — the analytical observer seat supplies this attestation. Note the limit: no corroborating source from the contributing side attests that the CURRENT terms remain the right answer to that original problem; the problem's reality and the terms' fairness are attested by different seats, and conflating them would be a cover-story move.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__constitutional_impossibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__constitutional_impossibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__constitutional_impossibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, ExtMetricName, E),
    domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(secession_legitimacy_boundary__constitutional_impossibility_reading),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.38 because this reading assesses the standing arrangement by its own lights: membership obligations on higher-revenue provinces are real but classified as legitimate federation dues, and the extraction characterization that would justify exit is rejected as a category error. The value is not zero because the reading cannot deny the growing cost asymmetry it observes — only its legitimacy. Suppression is 0.68 as a RAW structural property (unscaled by power or scope, per the framework rule): the exit channel requires simultaneous dual supermajorities, clarity conditions were legislated after referenda rather than before, courts refuse effect to out-of-channel attempts, and the international system withholds recognition from unilateral exits. It stops short of higher values because movements operate openly, referenda are legally tolerated, and no criminalization machinery exists in the liberal-federation cases this reading describes. Theater_ratio 0.40: the adjudicative apparatus functions daily and genuinely, but the negotiated-exit door has never opened anywhere, and official rhetoric continues directing aggrieved provinces toward it — a meaningful performative share. Accessibility_collapse 0.52: once the design is understood, unilateral and internationally-recognized alternatives collapse heavily, but the formal amendment route remains visibly open on paper, retaining partial accessibility. Resistance 0.58: serial referenda, sustained movements, litigation, and recurring scholarly contestation, without armed rebellion in the modern cases. The temporal series run on ONE shared grid (points 0, 6, 12, 18, 24, 30; T0 approximates the modern consolidated amendment framework, T12-T18 the secession-referendum era and subsequent clarity-statute hardening, T30 the contemporary period). suppression_requirement is tracked because the story specifically traces enforcement-capacity BUILD-UP — the step-change at T12-T18 models clarity legislation and doctrinal consolidation, an enforcement ratchet, not mere extraction drift; base_extractiveness rises gently with transfer-asymmetry growth (relevant to T17 abductive monitoring on a mountain-claimed story); theater climbs as cumulative disuse of the exit door outpaces the rhetoric pointing at it.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently and should. From the federal and judicial seats the arrangement presents as constitutional bedrock — permanence experienced as the shape of the world rather than a rule anyone chose. From the contributor province it presents as a gilded enclosure: real services, real countercyclical insurance, real market access, and an exit that exists everywhere except in practice. From the separatist seat it presents as categorical closure — the rule's legitimacy claim is exactly what the movement exists to contest, so no framing of the rule can register as neutral there. Same-power lateral divergence: the contributor and recipient provinces hold identical nominal constitutional standing and opposite net positions; the same rule binds one as principal payer and the other as payee, and their exit values differ accordingly (the payee loses receipts by leaving the pool; the payer gains release). Inter-institutional divergence: federal and provincial orders sit inside one constitutional framework yet experience the amendment gate oppositely — as protective supermajoritarianism from the center, as a bolted door from the resource-heavy province. The engine computes these divergences from the structural data; the authored mountain claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (federal_government, recipient_provinces, rest_of_federation_electorates) derive low directionality through the standard chain. The victims array is INTENTIONALLY EMPTY: the constitutional-impossibility reading denies the extraction characterization, and per the kernel rules the victim sets of the sibling readings belong to their files, not this one — folding them in would average across readings and violate epsilon invariance. But this leaves the structural position of the principal cost-bearer invisible to array-based derivation: resource_rich_net_contributor_province would otherwise derive a mid-low directionality indistinguishable from incidental participants. The override (powerful -> 0.78) restores the structural fact — largest net outflows in the federation, exit gated behind never-assembled dual supermajorities — WITHOUT importing any sibling reading's legitimacy judgment. Directionality records position (who bears and who receives); legitimacy stays in epsilon, where it is reading-indexed. Provincial_separatist_movements reach the target end through identity-locked exit modulation rather than array declaration, which keeps the reading's clean victim-free structure intact while still encoding that the prohibition binds them most directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — union fragility under unilateral withdrawal — remains live, so this is not a dead-mandate case; the mismatch consumer reads founding_problem_status=live x disappearance_verdict=world_rearranges and finds no zombie flag. The live mandatrophy-adjacent risk runs in the OPPOSITE direction from obsolescence: naturalization. This reading dresses a constructed, actively enforced allocation rule in constitutive permanence — which is precisely the false-summit signature. Declaring beneficiaries on the mountain claim routes the file through false-summit evaluation, and the natural_law_vs_constructed_permanence omega carries the necessity-versus-design question the schema requires. Symmetrically, the classification guards the mirror-image error available to the sibling readings: reading pure extraction where a live, load-bearing coordination function operates. Seat-divergent computation is what separates the two failure modes — the same structure reads as constitutive order from the agenda-setter seats, as legitimate dues from the beneficiary seats, and as a bolted enclosure from the contributor seat; only the per-seat computation, not the authored claim, locates where on that spectrum the arrangement actually operates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_permanence,
    'Is the union-permanence principle a structural feature of viable federation (closer to natural law for this domain) or a designed allocation rule that identifiable agents benefit from treating as inevitable?',
    'Comparative test: federations that codified member exit rights (explicit secession clauses) versus those that forbade them. If codified-exit federations persist at rates comparable to permanence-clause federations, permanence is design choice rather than structural necessity.',
    'If design, the mountain claim is a false summit: the constraint recomputes as a constructed, actively enforced arrangement with identifiable beneficiaries, and the reading''s constitutive framing fails. If necessity, the constitutive reading survives and the authored suppression reflects the irreducible price of the commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_permanence, empirical, 'Whether permanence is structural necessity or constructed rule (FSM ambiguity carrier).').

omega_variable(
    amendment_route_functionality,
    'Is the negotiated amendment exit a reachable alternative, or a formally open, practically jammed door?',
    'Enumerate every attempted amendment-route exit across comparable federations; compute the supermajority constellations each design requires versus the constellations ever politically assembled; test whether any design parameter (e.g. simultaneous dual assent by the exiting unit and the remainder) makes success arithmetically implausible.',
    'If jammed, theater_ratio is understated and the arrangement operates closer to pure prohibition behind a legitimating facade. If reachable, the negotiated-exit framing earns its keep and part of the measured cost-bearing is the price of a real option.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_route_functionality, empirical, 'Reachability of the formal exit channel.').

omega_variable(
    reading_index_epsilon_divergence,
    'This file instantiates one reading (constitutional impossibility) of the secession_legitimacy_boundary kernel; the sibling readings (popular sovereignty, grievance threshold, treaty primacy) authorize different beneficiary/victim sets and different epsilon over the SAME standing arrangement — which element of the disagreement drives the divergence?',
    'Cross-file comparison at kernel level: locate the disagreement in the legitimacy source of exit (ratified text versus referendum result versus injustice threshold versus treaty-holder consent), not in the arrangement''s mechanics, which all four files describe identically.',
    'This file''s epsilon (0.38) and empty victim set are indexed to THIS reading. Treating them as topic-level verdicts would corrupt cross-reading meta-analysis. Sibling files carry their own victim sets: referendum-denied majorities under popular sovereignty, threshold-crossing populations under grievance threshold, unconsulted treaty holders under treaty primacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_index_epsilon_divergence, conceptual, 'Committer-frame routing: epsilon and victim structure are reading-indexed, not topic truth.').

omega_variable(
    clarity_conditions_legitimacy,
    'Do clarity conditions imposed after referenda strengthen the amendment channel''s legitimacy, or reveal goalpost-moving that further jams the negotiated exit?',
    'Sequence audit: compare conditions demanded before versus after each referendum; test whether a hypothetical perfectly clear majority would in fact have triggered negotiation under the rules as they stood at the time the vote was held.',
    'If goalpost-moving, the negotiated-exit justification loses evidentiary support, effective suppression runs above the authored scalar, and the reading''s claim that a legitimate channel exists weakens. If applied in good faith, the channel''s legitimacy stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(clarity_conditions_legitimacy, empirical, 'Whether post-hoc clarity rules are good-faith application or retroactive tightening.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__constitutional_impossibility_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(sece_tr_t6, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(sece_tr_t12, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(sece_tr_t18, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 18, 0.37).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(sece_tr_t30, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(sece_be_t6, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(sece_be_t12, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(sece_be_t18, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 18, 0.36).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(sece_be_t30, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(sece_su_t6, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 6, 0.54).
narrative_ontology:measurement(sece_su_t12, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(sece_su_t18, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 18, 0.64).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(sece_su_t30, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__constitutional_impossibility_reading, resource_allocation).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'secession legality' decomposes under the epsilon-invariance principle (DP-001) into four structurally distinct constraints sharing one kernel, each with its own epsilon, beneficiary/victim structure, and classification: this file (constitutional impossibility — amendment-exclusive exit; reading-indexed epsilon 0.38; empty victim set because the reading denies the extraction claim); popular_sovereignty_reading (referendum self-legitimation; victim set = majorities whose referendum results are denied effect); grievance_threshold_reading (injustice-conditional legitimacy; victim set = populations past the structural-injustice threshold); treaty_primacy_reading (consent-preconditioned legitimacy; victim set = unconsulted treaty holders). Family topology: this reading functions as the legal baseline AGAINST WHICH the grievance-threshold reading defines its departures (upstream in citation structure); treaty primacy sits upstream historically (validity claims predating both constitutional orders) and exerts structural pressure on this file's ratification formulas through duty-to-consult jurisprudence without foreclosing it. Every family member links the others via affects_constraints; orphan stories within this kernel are a code smell.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secession_legitimacy_boundary__constitutional_impossibility_reading, powerful, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
