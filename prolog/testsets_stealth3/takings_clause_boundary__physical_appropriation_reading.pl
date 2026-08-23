% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__physical_appropriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__physical_appropriation_reading, []).

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
 *   constraint_id: takings_clause_boundary__physical_appropriation_reading
 *   human_readable: Takings Compensation Boundary - Physical Appropriation Reading
 *   domain: constitutional/legal-political
 *
 * SUMMARY:
 *   The constraint is the payment boundary drawn by one reading of the
 *   constitutional compensation clause: government owes property owners
 *   payment exactly when it physically seizes or permanently occupies their
 *   holdings, and owes nothing for regulation that merely destroys value. The
 *   arrangement coordinates (one bright line replacing per-ordinance payment
 *   disputes, an absolute anti-seizure guarantee for every owner) and
 *   extracts (a century of expanding regulatory intensity has moved
 *   ever-larger uncompensated losses onto the owners of restricted parcels).
 *   This file is one member of a three-story family decomposing the
 *   colloquial label 'the Takings Clause': this reading's epsilon reflects a
 *   narrow compensated class and a wide uncompensated loss-bearing class; the
 *   regulatory-takings sibling authors a far wider compensated class and
 *   correspondingly lower uncompensated transfer; the categorical sibling
 *   sits between, with per se triggers plus factor-balancing residue. The
 *   claim/metric split is deliberate: the constraint is CLAIMED tangled_rope
 *   (genuine coordination bargain plus asymmetric extraction, actively
 *   enforced), while the metrics describe its measured operation
 *   independently.
 *
 * KEY AGENTS:
 *   - constitutional_judiciary: agenda setter (institutional/analytical-exit) - writes and polices the payment boundary through doctrine
 *   - owners_with_uncompensated_regulatory_losses: primary target (moderate/trapped) - bears the uncompensated losses the boundary externalizes
 *   - physically_dispossessed_property_owners: protected beneficiary (moderate/trapped) - the narrow compensated class
 *   - general_taxpayers: diffuse beneficiary (organized/mobile) - receives the avoided compensation expenditure
 *   - municipal_planning_authorities and state_legislatures: institutional beneficiaries - regulate at zero compensation cost
 *   - institutional_land_developers: dual-positioned (powerful/arbitrage) - collects scarcity rents, absorbs compliance costs
 *   - owner_occupants_lacking_litigation_capacity: excluded seat (powerless/trapped) - bears losses without voice
 *   - constitutional_scholars: analytical observer - maps the boundary's history and contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, 0.68).
domain_priors:suppression_score(takings_clause_boundary__physical_appropriation_reading, 0.65).
domain_priors:theater_ratio(takings_clause_boundary__physical_appropriation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__physical_appropriation_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__physical_appropriation_reading, "Takings Compensation Boundary - Physical Appropriation Reading").
narrative_ontology:topic_domain(takings_clause_boundary__physical_appropriation_reading, "constitutional/legal-political").

domain_priors:requires_active_enforcement(takings_clause_boundary__physical_appropriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__physical_appropriation_reading, 'b21edffa-2e0c-4056-a13f-e5f8012f417f').
narrative_ontology:cs_kernel_codification('b21edffa-2e0c-4056-a13f-e5f8012f417f', fixed_text).
narrative_ontology:cs_authority_grounding('b21edffa-2e0c-4056-a13f-e5f8012f417f', lineage).
narrative_ontology:cs_interpretation_layer_present('b21edffa-2e0c-4056-a13f-e5f8012f417f').
narrative_ontology:cs_reading_relation('b21edffa-2e0c-4056-a13f-e5f8012f417f', takings_clause_boundary__regulatory_takings_reading, influences).
narrative_ontology:cs_reading_relation('b21edffa-2e0c-4056-a13f-e5f8012f417f', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_axiom('b21edffa-2e0c-4056-a13f-e5f8012f417f', foundational, just_compensation_exclusive_to_physical_appropriation).
narrative_ontology:cs_axiom_status(just_compensation_exclusive_to_physical_appropriation, holdable).
narrative_ontology:cs_axiom_grounding('b21edffa-2e0c-4056-a13f-e5f8012f417f', just_compensation_exclusive_to_physical_appropriation, conventional).
narrative_ontology:cs_axiom('b21edffa-2e0c-4056-a13f-e5f8012f417f', secondary, regulatory_diminution_is_background_risk).
narrative_ontology:cs_axiom_status(regulatory_diminution_is_background_risk, holdable).
narrative_ontology:cs_axiom_grounding('b21edffa-2e0c-4056-a13f-e5f8012f417f', regulatory_diminution_is_background_risk, instrumental).
narrative_ontology:cs_reference_frame('b21edffa-2e0c-4056-a13f-e5f8012f417f', physical_appropriation_only_baseline).
narrative_ontology:cs_drift_state('b21edffa-2e0c-4056-a13f-e5f8012f417f', contemporary, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('b21edffa-2e0c-4056-a13f-e5f8012f417f', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, physically_dispossessed_property_owners).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, general_taxpayers).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, municipal_planning_authorities).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, owners_with_uncompensated_regulatory_losses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, state_legislatures).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, institutional_land_developers).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, institutional_land_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Fifth Amendment compensation clause, decides which government interferences with property count as appropriations that must be paid for, and writes the doctrine that separates paid seizure from unpaid regulation. Inherits the constitutional text and transmits its reading across generations; there is no exit from this position because the judiciary is the enforcement site itself.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, constitutional_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Own land or buildings that government condemns, seizes, or permanently occupies for roads, facilities, or access. Under the payment rule they receive mandated payment when dispossession occurs, and the loss is shifted to public budgets. Before a seizure they cannot decline the guarantee's terms; after it they leave the holding entirely, compensated.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, physically_dispossessed_property_owners, beneficiary,
    moderate, biographical, trapped, national).

% Own parcels whose value or permitted use is cut by downzonings, environmental setbacks, wetlands designations, or preservation freezes. The payment rule pays them nothing for these losses. Selling realizes the loss at the suppressed price; litigation against the rule rarely restores value and is slow and expensive; political appeals take years. The diminished value sits on their balance sheet until they exit.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, owners_with_uncompensated_regulatory_losses, payer,
    moderate, biographical, trapped, national).

% Are spared the tax bill that itemized payment for every restrictive ordinance would require; the benefit arrives invisibly as forgone public expenditure. They can move between jurisdictions with different regulatory appetites, though the federal boundary follows them everywhere.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, general_taxpayers, beneficiary,
    organized, generational, mobile, national).

% Impose use restrictions, setbacks, and preservation overlays on private parcels with no budgetary exposure for the value destroyed. The affordability of their entire regulatory toolkit depends on the payment boundary staying where it is; they are bound by the same constitutional order whose slack they rely on.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, municipal_planning_authorities, beneficiary,
    institutional, generational, constrained, regional).

% Enact statewide regulatory regimes over shoreline, agricultural land, and habitat at zero compensation cost under the payment boundary. When owner backlash builds, some legislate partial compensation statutes of their own accord, adjusting the burden without touching the federal boundary.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, state_legislatures, beneficiary,
    institutional, generational, constrained, regional).

% Large holders and builders. Restrictions that constrain smaller rivals raise the value of land they already hold entitled; their own projects meanwhile absorb compliance costs, delay, and redesign. They shop jurisdictions, buy variances, assemble exemption packages, and lobby for bespoke carve-outs - mobility that small owners do not have.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, institutional_land_developers, beneficiary,
    powerful, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__physical_appropriation_reading, institutional_land_developers, payer).

% Owner-occupants of modest homes and farms hit by downzonings or preservation freezes. No legal budget carries them into a compensation fight; their losses register nowhere but their own equity. They would demand either payment or relief if they had a seat, and their absence keeps the recorded opposition to the boundary thinner than the real opposition.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, owner_occupants_lacking_litigation_capacity, excluded,
    powerless, immediate, trapped, local).

% Produce the interpretive tradition the judiciary draws on: histories of the clause's drafting, mappings of its doctrinal movement, arguments over where the payment boundary properly sits. They hold no parcel and collect nothing from either outcome.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__physical_appropriation_reading, general_taxpayers).
narrative_ontology:fixing_cost_class(takings_clause_boundary__physical_appropriation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles, with one enforceable bright line, when the sovereign owes payment for interfering with property: every owner holds an unconditional guarantee of payment against physical dispossession, and government holds a predictable, fiscally bounded zone of regulatory authority. The line replaces case-by-case payment negotiation over every ordinance with a single settled rule.
% TRANSFER_FUNCTION: Moves the economic burden of regulatory value destruction - downzonings, use restrictions, setback mandates, preservation freezes - from public budgets onto the affected private owners, unpaid; and guarantees full payment to owners whose property is physically seized. Net flow: concentrated unpaid losses from regulated owners, diffused avoided costs to taxpayers and program beneficiaries.
% ABSENT_VOICES: Resource-poor owner-occupants absorb losses without ever entering the record; they surface only when a funded lawyer finds them. The public at large never sees an itemized bill for any specific regulatory choice made free of charge, so no constituency forms around the true cost of individual restrictions. Future purchasers who inherit encumbered parcels at prices that partially capitalize the risk are likewise unrepresented when the boundary is drawn.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight - if every regulatory diminution suddenly demanded payment - land-use control would freeze or migrate to outright purchase of interests; municipal and state budgets would face open-ended contingent liability; housing supply, conservation practice, and infrastructure siting would reorganize around negotiated or bought restrictions. The physical-seizure guarantee would survive in statute, but the unpaid-regulation half is load-bearing for the entire administrative state.
% FOUNDING_PROBLEM: Foreclosing the sovereign's power to seize private property without payment - the revolutionary-era grievance of arbitrary confiscation, forced quartering, and impressment that the compensation clause was written to make impossible.
% FOUNDING_PROBLEM_CORROBORATION: Founding-era legal histories document confiscation practice as the drafting motive, attested from archival sources outside this reading's beneficiary coalition; state condemnation records show seizure orders issuing continuously in every jurisdiction, keeping the physical-dispossession problem operative today. No source outside the beneficiary set attests the founding problem is dead.
narrative_ontology:disappearance_verdict(takings_clause_boundary__physical_appropriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__physical_appropriation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__physical_appropriation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(takings_clause_boundary__physical_appropriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__physical_appropriation_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__physical_appropriation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__physical_appropriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 because the boundary's operative effect is a large and growing unpaid transfer: regulatory regimes that destroy most of a parcel's use value proceed at zero public cost, while the compensated class (physical dispossessees) is narrow. Suppression is authored at 0.65 as a raw structural property (unscaled by power or scope): owners facing losses have no exit that preserves value - selling realizes the loss, litigation rarely reverses it - and the claim-rejection machinery must actively defeat each new compensation demand. Theater_ratio 0.30: the gatekeeping function is real and decisive, but a growing share of the boundary's public life is founding-era rhetoric detached from a physical-seizure caseload that has shrunk relative to the regulatory caseload it rhetorically eclipses. Accessibility_collapse 0.65: for the individual owner, once the rule is understood, the alternative (converting a regulatory loss into a claim) collapses almost entirely; at the system level, alternatives persist (amendment, judicial turnover, state statutes), so collapse is partial. Resistance 0.60: the boundary has met a sustained century of counterpressure - litigation waves, state compensation statutes, ballot initiatives, an organized property-rights movement. All three temporal series run on one shared grid (t=0..100, six points); enforcement requirement rises monotonically as each compensation-claim wave was beaten back with firmer doctrinal commitment.
 *
 * PERSPECTIVAL GAP:
 *   From the payer seat, the constraint is experienced as denial machinery: each refused claim is an unrecoverable personal loss absorbed in silence, and the same doctrine that pays a condemned neighbor pays him nothing. From the beneficiary seats it is experienced as the opposite: the guarantee that makes ownership safe from seizure at all, and the fiscal discipline that keeps regulation affordable. From the judiciary seat it is stewardship of a founding text. The engine computes these divergent per-seat classifications from the structural data; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Physically dispossessed owners sit nearest the beneficiary pole: the constraint's core promise pays them in full, converting their worst-case loss into public expense. General taxpayers and the planning/legislative seats sit low-d: spared costs, retained regulatory power. Owners bearing regulatory losses sit near the full-target pole: the identical structure that indemnifies seizure strips them of compensation for diminution, and their trapped exit amplifies effective extraction. Developers straddle mid-range via their dual beneficiary/payer declaration. Suppression is carried unscaled into the computation; only extractiveness is scaled, by directionality and by the national scope that makes verification of diffuse losses harder and thus amplifies effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - preventing uncompensated confiscation - is live, not dead: condemnation proceeds continuously, so the arrangement has not outlived its mandate and carries no sunset. The tangled_rope classification is what prevents mislabeling in both directions: reading the boundary as a pure rope would erase the concentrated unpaid losses that define the payer seat's experience; reading it as a snare would erase the real anti-seizure guarantee every owner holds and the genuine bright-line coordination function. The R5 mismatch consumer finds status=live crossed with verdict=world_rearranges - a consistent profile, no zombie flag: the parties depend on the arrangement and the problem it was built for still exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_trigger_location_contest,
    'This constraint is the physical_appropriation_reading of the takings_clause_boundary kernel; what structurally changes if a sibling reading displaces it?',
    'Track doctrinal trajectory and amendment attempts: adoption of the regulatory_takings_reading would widen the compensated class to every substantially diminished owner, collapsing this constraint''s uncompensated-transfer channel and shifting enforcement from claim-denial to valuation administration; adoption of the categorical_reading would split the difference with per se categories plus balancing.',
    'Sibling displacement rewrites the victim set and the epsilon of the compensation regime wholesale; the current story''s classification is valid only for the physical-appropriation instantiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_trigger_location_contest, conceptual, 'Committer structure: one reading of a contested kernel; the contest lives entirely in the trigger condition.').

omega_variable(
    background_risk_vs_concentrated_extraction,
    'Is the unpaid regulatory loss borne by owners fairly distributed background risk, as this reading asserts, or systematic extraction concentrated on non-consenting minority owners?',
    'Distributional audit across regimes and decades: who bears the losses (parcel classes, income strata, regions) versus who captures the regulatory benefits; if losses concentrate on politically weak owner classes while gains diffuse to organized beneficiaries, the extraction reading of the arrangement strengthens.',
    'A concentration finding pushes computed classification toward the snare end of the family; a genuine diffusion finding supports treating the unpaid channel as the premium on the anti-seizure guarantee.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(background_risk_vs_concentrated_extraction, conceptual, 'Whether the payer class experiences background risk or targeted extraction.').

omega_variable(
    cedar_point_revival_durability,
    'Is the recent widening of the physical-occupation trigger - access regulation reclassified as occupation requiring payment - a durable recentering of the kernel toward this reading, or a bounded exception?',
    'Follow subsequent access, easement, and temporary-entry cases: if courts extend the occupation framing across the regulatory frontier, the reading''s operative scope widens durably; if applications stall at edge cases, the revival is bounded.',
    'A durable revival shifts the victim-set composition toward access-affected owners and steepens the extractiveness trajectory authored in the measurement series.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cedar_point_revival_durability, empirical, 'Durability of the physical-occupation trigger''s recent expansion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__physical_appropriation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t0, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(taki_tr_t20, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(taki_tr_t40, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(taki_tr_t60, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(taki_tr_t80, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 80, 0.27).
narrative_ontology:measurement(taki_tr_t100, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(taki_be_t0, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(taki_be_t20, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(taki_be_t40, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(taki_be_t60, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(taki_be_t80, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 80, 0.63).
narrative_ontology:measurement(taki_be_t100, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t0, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(taki_su_t20, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(taki_su_t40, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(taki_su_t60, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(taki_su_t80, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 80, 0.61).
narrative_ontology:measurement(taki_su_t100, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__physical_appropriation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary__regulatory_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary__categorical_takings_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial 'Takings Clause' label per the epsilon-invariance principle: the label conflates three structurally distinct compensation-boundary claims. This story (physical_appropriation_reading) is the historical upstream baseline (narrow trigger, wide unpaid zone); regulatory_takings_reading is the downstream expansion attempt (value-destruction trigger); categorical_takings_reading is the synthesis (per se triggers plus balancing). Each member authors its own epsilon, victim set, and enforcement profile; edges run from this baseline to both dependents because the baseline's persistence constrains what the descendants can claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
