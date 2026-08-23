% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__commemorative_husk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__commemorative_husk, []).

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
 *   constraint_id: stone_land_use_rule__commemorative_husk
 *   human_readable: Flood-Runup Stone as Commemorative Husk (Land-Use Rule, Lapsed)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   A stone raised after a catastrophic flood at the measured runup line,
 *   inscribed with a prohibition on habitation below it, governed the
 *   waterfront's land use for generations: parcels were sited and prices set
 *   with reference to the line. The enforcement machinery has since dissolved
 *   — the setback ordinance was repealed, the stone was re-designated a
 *   heritage object, and the annual ceremony now commemorates the dead rather
 *   than addresses the living. Buildings rise below the old line at market
 *   pace; the stone stands in the plaza of a new waterfront quarter as
 *   'historic character.' This story authors the standing arrangement by this
 *   reading's own lights: a land-use regime with zero behavioral force,
 *   maintained as performance, whose vacancy is priced as waterfront
 *   convenience by those who capture it now and whose risk is carried by
 *   those who will occupy the buildings later. Constraint family: the
 *   colloquial label 'the flood stone's rule' decomposes into two
 *   structurally distinct constraints — the live prohibition (sibling story,
 *   near-zero epsilon, coordination with no victim set) and this
 *   commemorative husk (epsilon 0.74 over the vacancy arrangement). Same
 *   inscription, different epsilon: two files, linked by
 *   network.affects_constraints; the sibling reading is the reference state
 *   this reading measures its drift from. KEY AGENTS (by structural
 *   relationship): municipal_council: agenda-setter
 *   (institutional/constrained) — holds the regulatory pen, repealed the
 *   ordinance, defers the risk past its horizon;
 *   municipal_heritage_committee: agenda-setter of the memorial layer
 *   (moderate/identity_locked) — administers the stone's meaning, custodial
 *   identity fused with stewardship; waterfront_developers: primary
 *   beneficiary (powerful/arbitrage) — captures hazard-zone land value priced
 *   as safe; future_hazard_zone_residents: primary target (powerless/trapped)
 *   — bear the transferred runup risk, no seat, some unborn;
 *   waterfront_business_occupants: dual-positioned beneficiary and bearer of
 *   tail risk (organized/constrained); flood_survivor_association: excluded
 *   voice (organized/identity_locked) — heard annually, seated nowhere;
 *   regional_planning_authority: excluded analyst (institutional/constrained)
 *   — maps the hazard, advisory only; memorial_tourism_operators: residual
 *   beneficiary (moderate/mobile) — monetizes the husk as attraction;
 *   regional_insurance_pool_members: diffuse payers (powerless/constrained) —
 *   absorb the socialized losses.
 *
 * KEY AGENTS:
 *   - municipal_council: agenda-setter (institutional/constrained) — repealed the setback ordinance, collects waterfront tax growth, defers catastrophe past the electoral horizon
 *   - municipal_heritage_committee: agenda-setter of the memorial layer (moderate/identity_locked) — administers the stone's meaning; its mandate exists because the stone exists
 *   - waterfront_developers: primary beneficiary (powerful/arbitrage) — captures hazard-zone land value priced as if safe; capital deploys elsewhere when returns turn
 *   - future_hazard_zone_residents: primary target (powerless/trapped) — bear the transferred runup risk; structurally absent from every decision that created their exposure
 *   - waterfront_business_occupants: dual-positioned (organized/constrained) — collect the location premium now, carry the tail risk in sunk leaseholds
 *   - flood_survivor_association: excluded voice (organized/identity_locked) — testimony absorbed by the annual ceremony, petitions always filed
 *   - regional_planning_authority: excluded analyst (institutional/constrained) — its inundation maps show the quarter inside the envelope; advisory only
 *   - memorial_tourism_operators: residual beneficiary (moderate/mobile) — revenue depends on the husk being maintained and visited
 *   - regional_insurance_pool_members: diffuse payers (powerless/constrained) — premiums and relief backstops spread the waterfront's unhedged risk across the region
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, 0.74).
domain_priors:suppression_score(stone_land_use_rule__commemorative_husk, 0.35).
domain_priors:theater_ratio(stone_land_use_rule__commemorative_husk, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, extractiveness, 0.74).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, accessibility_collapse, 0.18).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__commemorative_husk, piton).
narrative_ontology:human_readable(stone_land_use_rule__commemorative_husk, "Flood-Runup Stone as Commemorative Husk (Land-Use Rule, Lapsed)").
narrative_ontology:topic_domain(stone_land_use_rule__commemorative_husk, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__commemorative_husk, '6a42ca1b-ef5a-477c-a9fb-e545e1366060').
narrative_ontology:cs_kernel_codification('6a42ca1b-ef5a-477c-a9fb-e545e1366060', fixed_text).
narrative_ontology:cs_authority_grounding('6a42ca1b-ef5a-477c-a9fb-e545e1366060', lineage).
narrative_ontology:cs_interpretation_layer_present('6a42ca1b-ef5a-477c-a9fb-e545e1366060').
narrative_ontology:cs_reading_relation('6a42ca1b-ef5a-477c-a9fb-e545e1366060', stone_land_use_rule__behavioral_competence, forecloses).
narrative_ontology:cs_axiom('6a42ca1b-ef5a-477c-a9fb-e545e1366060', foundational, behavioral_uptake_constitutes_rule_force).
narrative_ontology:cs_axiom_status(behavioral_uptake_constitutes_rule_force, holdable).
narrative_ontology:cs_axiom_grounding('6a42ca1b-ef5a-477c-a9fb-e545e1366060', behavioral_uptake_constitutes_rule_force, conventional).
narrative_ontology:cs_axiom('6a42ca1b-ef5a-477c-a9fb-e545e1366060', foundational, memorialization_does_not_retire_hazard).
narrative_ontology:cs_axiom_status(memorialization_does_not_retire_hazard, holdable).
narrative_ontology:cs_axiom_grounding('6a42ca1b-ef5a-477c-a9fb-e545e1366060', memorialization_does_not_retire_hazard, empirically_contingent).
narrative_ontology:cs_reference_frame('6a42ca1b-ef5a-477c-a9fb-e545e1366060', live_setback_prohibition).
narrative_ontology:cs_drift_state('6a42ca1b-ef5a-477c-a9fb-e545e1366060', contemporary, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('6a42ca1b-ef5a-477c-a9fb-e545e1366060', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__commemorative_husk, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, waterfront_developers).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, waterfront_business_occupants).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, memorial_tourism_operators).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, future_hazard_zone_residents).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, regional_insurance_pool_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, waterfront_business_occupants).
narrative_ontology:constraint_vindicates(stone_land_use_rule__commemorative_husk, commemorative_stewardship_substitutes_for_land_use_regulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the municipal land-use code. Repealed the waterfront setback ordinance that had referenced the stone's runup line and re-designated the stone as a heritage object under the parks budget. Collects rising waterfront tax revenue and the political credit for growth; the flood risk that accompanies the new buildings matures on a longer clock than its electoral term. Re-imposing the building line would require compensation for sunk waterfront investment and a fight with the development lobby, so the code stays as it is.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, municipal_council, agenda_setter,
    institutional, immediate, constrained, local).

% Administers the stone: cleaning, the bronze plaque, the annual remembrance ceremony, school visits. Its mandate and small budget exist because the stone exists; members describe custodial duty as fidelity to the founding generation. If the inscription were enforced as a land-use rule again, the committee's role would shrink from steward to footnote. It has never petitioned the council to restore the prohibition, and its ceremonies now describe the stone as a memorial to the dead rather than a rule for the living.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, municipal_heritage_committee, agenda_setter,
    moderate, generational, identity_locked, local).

% Assemble and build parcels below the old runup line, where land is priced as ordinary waterfront because nothing in the code says otherwise. The stone stands in the plaza of one of their projects as a heritage feature marketed as 'historic character.' Their capital is not tied to this shore: they price flood risk into each deal and deploy elsewhere when the risk-adjusted return turns, so the long-run consequence of building here is not theirs to hold.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, waterfront_developers, beneficiary,
    powerful, biographical, arbitrage, regional).

% Run the cafes, hotels, and shops of the new waterfront quarter. They collect the location premium daily — views, foot traffic, brand. Their leaseholds and fit-outs are sunk into buildings below the line; they know the gauge record, and a few carry extra inventory insurance, but leaving means writing off the investment. They sponsor the annual ceremony because it is good for trade.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, waterfront_business_occupants, beneficiary,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__commemorative_husk, waterfront_business_occupants, payer).

% Run guided walks, a small museum room, and anniversary events built around the stone. Their revenue exists because the stone is maintained and visited. They have no stake in whether anyone builds below the line and would lose their product if the stone were removed — or, in the other direction, if the building line came back and the waterfront quarter came down.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, memorial_tourism_operators, beneficiary,
    moderate, biographical, mobile, local).

% The people who will live in the buildings now rising below the runup line — most have not yet arrived and some are not yet born. They will pay market price for homes whose risk was never priced, inherit an evacuation problem designed without them, and hold no seat in any decision now being made. Their only appearance in the record is as projected occupancy figures in development pro formas.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, future_hazard_zone_residents, payer,
    powerless, generational, trapped, local).

% Households and businesses across the region whose premiums and taxes feed the pooled insurance and disaster-relief backstop. When the waterfront floods, their pool pays, and premiums across the region ratchet afterward. They are diffuse, uncoordinated, and mostly unaware that a stone two towns over once marked where building was supposed to stop.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, regional_insurance_pool_members, payer,
    powerless, biographical, constrained, regional).

% Aging members of the families that lost the old waterfront quarter. They attend and speak at the annual ceremony, keep the archive of photographs and names, and have repeatedly asked the council to restore the building line. Their requests are received as ceremony input and filed. Membership shrinks each year; the archive is an obligation they cannot put down without betraying the dead.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, flood_survivor_association, excluded,
    organized, generational, identity_locked, local).

% Produces the regional flood-extent and runup models that show the new waterfront quarter inside the projected inundation envelope. Under the municipal code its maps are advisory: it comments on permit applications, and its comments are noted and overruled. It keeps mapping. It cannot veto, cannot levy, and has no seat at the council vote.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, regional_planning_authority, excluded,
    institutional, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(stone_land_use_rule__commemorative_husk, waterfront_developers).
narrative_ontology:fixing_cost_class(stone_land_use_rule__commemorative_husk, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone once solved a real land-use coordination problem: holding settlement off the flood runup zone when each builder could otherwise capture the waterfront premium and shift catastrophe risk onto neighbors and successors. In its present state the arrangement solves no land-use problem at all; its remaining coordination work is commemorative — it fixes a time and place for shared remembrance and gives heritage tourism a focal point.
% TRANSFER_FUNCTION: Moves hazard-zone land value to developers and waterfront occupants, priced as if the runup line were history rather than hydrology; moves the corresponding flood risk to future residents and to regional insurance pools; moves municipal budget and civic attention into plaque maintenance and an annual ceremony; moves survivor testimony into a once-a-year slot where it carries no planning weight.
% ABSENT_VOICES: The future residents of the buildings now rising below the line — unarrived, some unborn — would object and cannot; they appear only as occupancy projections. The regional planning authority's inundation maps are filed as advisory comments and overruled. The flood survivor association speaks once a year at the ceremony, which is the arrangement's own mechanism for hearing them without seating them.
% DISAPPEARANCE_RATIONALE: The land-use outcome would not change: buildings would rise below the line at the same pace, which is this reading's defining claim. But a real micro-economy is organized around the husk — the heritage committee's mandate, the tourism operators' product, the ceremony's calendar — and it would collapse. The community would also lose its last ritual friction against total hazard amnesia: whatever the stone fails to do about building, it currently does do about remembering, and the remembering is what a future council would need in order to re-impose the line.
% FOUNDING_PROBLEM: After the flood that destroyed the old waterfront quarter, the stone was raised at the measured runup line with an inscription prohibiting habitation below it. The founding problem: individual builders capture the waterfront premium while the costs of the inevitable flood fall on neighbors, successors, and the dead's own descendants; the stone made the line a shared, self-evident boundary that needed no per-decision enforcement.
% FOUNDING_PROBLEM_CORROBORATION: The development coalition attests the founding problem is solved by modern engineered defenses and insurance, and therefore dead in its original form. The regional planning authority's inundation models, the tide-gauge record of the near-miss event that reached the old line within living memory, and the post-event premium ratchet all attest — from outside the benefiting parties — that the underlying problem is live and growing. The dispute is genuine and tracks the seats.
narrative_ontology:disappearance_verdict(stone_land_use_rule__commemorative_husk, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__commemorative_husk, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__commemorative_husk, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(stone_land_use_rule__commemorative_husk, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__commemorative_husk, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__commemorative_husk_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(stone_land_use_rule__commemorative_husk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.74) because the arrangement transfers runup risk wholesale to parties with no seat — future occupants and regional insurance pools — while the land value it frees is captured now; the husk itself enforces nothing, so the entire epsilon is vacancy-extraction, parasitic on a constraint that stopped binding. Suppression (0.35) is not coercion — no one is forced to do anything by the stone — but the husk occupies the institutional slot a live rule would need: the ceremony discharges the community's felt memory obligation, the heritage designation supplies the ready answer 'we already honor the flood' to any re-institution proposal, and property-rights machinery raises the cost of re-imposition. Theater (0.82): plaque maintenance, the annual rite, school visits, and the tourism product are nearly the whole of the stone's active life; its behavioral force is zero. Accessibility collapse (0.18): understanding the husk opens the waterfront completely — there is nothing to route around; the residual collapse is perceptual, false assurance collapsing the perceived need for alternatives. Resistance (0.2): the survivor association's annual petition and the planning authority's filed objections, episodic and always overruled; the victim classes cannot coalition — one has not arrived yet, the other is unorganized by construction — which is part of why the vacancy persists. Claim/metric independence: claimed_type is piton because the husk's own maintenance economics are piton economics — no maintainer profits from it, enforcement is zero, persistence is inertia plus ceremony — while the metrics describe the arrangement the vacancy enables; the divergence between the inert husk and the extractive vacancy is the finding, not an inconsistency to reconcile. The measurement series runs on one shared grid: enforcement requirement fell 0.70 to 0.04 while theater rose 0.10 to 0.82 and extraction accumulated 0.15 to 0.74; the enforcement-decay trajectory is the story's spine, which is why suppression_requirement is tracked rather than left as a scalar.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply on the same stone. From the heritage committee's seat the arrangement is sacred custodial duty; the committee does not experience a vacancy because its identity is the maintenance itself. From the developer's seat the stone is scenery — a heritage feature that adds sale value at zero regulatory cost. From the survivor association's seat the ceremony is both tribute and dispossession: their testimony is heard once a year and seated nowhere. From the council's seat the arrangement is a growth story with the risk deferred past the electoral horizon. From the future resident's seat there is no seat. The engine computes these divergences from power, exit, and role; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (waterfront_developers, waterfront_business_occupants, memorial_tourism_operators) sit near the beneficiary end — the arrangement subsidizes them: land priced without the runup risk, location premiums, a tourism product. Victims (future_hazard_zone_residents, regional_insurance_pool_members) sit near the target end — they bear the transferred risk, and the primary victim class is trapped by not existing yet: it cannot exit a decision it was never entered into. The municipal_council sits mid-low: it collects tax base and political credit now and defers the catastrophe past its horizon. The anomaly is the municipal_heritage_committee: an agenda-setter with custodian costs and no capture, held in place by identity-lock rather than gain. No directionality overrides are authored: the declared roles and exits carry the derivation, and an override keyed to the committee's power atom would also distort the tourism operators who share it.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is inverted: usually a mandate outlives its problem; here the problem has outlived its mandate — runup-zone settlement is larger than at founding while the arrangement's response has lapsed to zero. The classification prevents two mislabels. Calling the husk a rope would credit it with the land-use coordination it no longer performs. Calling it a snare would credit it with coercion it no longer exercises — nothing is enforced, no exit is barred by the stone, and the developers who capture the gains do not maintain it: bulldoze the stone tomorrow and their business is unaffected; the husk is kept by duty, not by profit. The piton reading locates the extraction where it actually flows — through the vacancy, captured by those the vacancy subsidizes, laundered by a ceremony that converts an unaddressed hazard into an honored memory. The theater series records the crossing: function and performance crossed near t=36, the Goodhart point where remembering the rule replaced keeping it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_status_empirical_settlement,
    'This story instantiates the commemorative_husk reading of kernel stone_land_use_rule; the sibling behavioral_competence reading holds the stone still binds. Which reading is empirically true of the stone''s present force — do building permits and siting decisions covary with the runup line or not?',
    'Permit-record and parcel-value analysis: regression of building siting, footprint, and finished-floor elevation on distance from the stone''s line, controlling for the waterfront premium; plus revealed-preference interviews with permit applicants on whether the stone entered any decision.',
    'If the sibling reading is true, this story''s epsilon (0.74) is misattributed — there is no vacancy, the beneficiary set dissolves into ordinary coordination costs, and the sibling file describes a live constraint this file wrongly declares lapsed; if this reading is true, the sibling story describes a constraint that no longer exists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_status_empirical_settlement, empirical, 'Whether the husk or the live-rule reading describes the stone''s present behavioral force.').

omega_variable(
    false_assurance_suppression,
    'Does the husk''s ceremonial presence actively suppress hazard preparedness — does the annual remembrance substitute for evacuation drills, building standards, and memory practice that would otherwise persist — or is the community''s preparedness decay independent of the memorial?',
    'Comparative preparedness indicators (drill frequency, evacuation-route knowledge, flood-insurance uptake) in matched communities with and without maintained hazard memorials, before and after near-miss events.',
    'If the husk actively suppresses preparedness, the authored suppression (0.35) understates the arrangement: the husk is not merely inert but mildly noxious, and the seat classifications shift toward the snare side of the ledger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_assurance_suppression, empirical, 'Whether commemoration suppresses preparedness or merely coexists with its decay.').

omega_variable(
    custodian_self_preservation,
    'Is the heritage committee''s maintenance of the stone driven by custodial duty alone, or by institutional self-preservation — would the committee''s mandate and budget survive de-accession of the stone, and does its identity depend on the husk persisting exactly as it is?',
    'Committee budget and remit history; council records on what happens to the parks heritage line if the stone is de-accessioned; interviews on the committee''s institutional self-understanding.',
    'If self-preservation is load-bearing, the husk has a covert maintainer-beneficiary and the piton reading weakens: a constraint maintained because a body feeds on it is closer to the snare side than the inertia side.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custodian_self_preservation, empirical, 'Whether the husk''s maintainer profits from maintenance.').

omega_variable(
    reinstitution_option_value,
    'Is the husk a scaffold-in-waiting — the memory anchor that would make re-imposing the building line cheap after the next near-miss — or an anesthetic, where each ceremony discharges the felt obligation to act and lowers the probability of re-institution?',
    'Comparative post-event policy trajectories in communities with maintained hazard memorials versus without: did the memorial community re-codify faster after a near-miss, or re-ceremonialize instead?',
    'If scaffold-in-waiting, the husk''s residual function carries option value and the authored suppression should be discounted; if anesthetic, the husk''s net contribution to the founding problem is negative and the theater_ratio understates the harm.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reinstitution_option_value, empirical, 'Whether commemoration preserves or discharges the obligation to re-institute the rule.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__commemorative_husk, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__commemorative_husk, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(ston_tr_t0, observed).
narrative_ontology:measurement(ston_tr_t12, stone_land_use_rule__commemorative_husk, theater_ratio, 12, 0.18).
narrative_ontology:measurement_basis(ston_tr_t12, observed).
narrative_ontology:measurement(ston_tr_t24, stone_land_use_rule__commemorative_husk, theater_ratio, 24, 0.34).
narrative_ontology:measurement_basis(ston_tr_t24, observed).
narrative_ontology:measurement(ston_tr_t36, stone_land_use_rule__commemorative_husk, theater_ratio, 36, 0.52).
narrative_ontology:measurement_basis(ston_tr_t36, observed).
narrative_ontology:measurement(ston_tr_t48, stone_land_use_rule__commemorative_husk, theater_ratio, 48, 0.7).
narrative_ontology:measurement_basis(ston_tr_t48, observed).
narrative_ontology:measurement(ston_tr_t60, stone_land_use_rule__commemorative_husk, theater_ratio, 60, 0.82).
narrative_ontology:measurement_basis(ston_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__commemorative_husk, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(ston_be_t0, observed).
narrative_ontology:measurement(ston_be_t12, stone_land_use_rule__commemorative_husk, base_extractiveness, 12, 0.22).
narrative_ontology:measurement_basis(ston_be_t12, observed).
narrative_ontology:measurement(ston_be_t24, stone_land_use_rule__commemorative_husk, base_extractiveness, 24, 0.38).
narrative_ontology:measurement_basis(ston_be_t24, observed).
narrative_ontology:measurement(ston_be_t36, stone_land_use_rule__commemorative_husk, base_extractiveness, 36, 0.52).
narrative_ontology:measurement_basis(ston_be_t36, observed).
narrative_ontology:measurement(ston_be_t48, stone_land_use_rule__commemorative_husk, base_extractiveness, 48, 0.65).
narrative_ontology:measurement_basis(ston_be_t48, observed).
narrative_ontology:measurement(ston_be_t60, stone_land_use_rule__commemorative_husk, base_extractiveness, 60, 0.74).
narrative_ontology:measurement_basis(ston_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(ston_su_t0, stone_land_use_rule__commemorative_husk, suppression_requirement, 0, 0.7).
narrative_ontology:measurement_basis(ston_su_t0, observed).
narrative_ontology:measurement(ston_su_t12, stone_land_use_rule__commemorative_husk, suppression_requirement, 12, 0.58).
narrative_ontology:measurement_basis(ston_su_t12, observed).
narrative_ontology:measurement(ston_su_t24, stone_land_use_rule__commemorative_husk, suppression_requirement, 24, 0.4).
narrative_ontology:measurement_basis(ston_su_t24, observed).
narrative_ontology:measurement(ston_su_t36, stone_land_use_rule__commemorative_husk, suppression_requirement, 36, 0.22).
narrative_ontology:measurement_basis(ston_su_t36, observed).
narrative_ontology:measurement(ston_su_t48, stone_land_use_rule__commemorative_husk, suppression_requirement, 48, 0.1).
narrative_ontology:measurement_basis(ston_su_t48, observed).
narrative_ontology:measurement(ston_su_t60, stone_land_use_rule__commemorative_husk, suppression_requirement, 60, 0.04).
narrative_ontology:measurement_basis(ston_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__commemorative_husk, identity_coordination).
narrative_ontology:affects_constraint(stone_land_use_rule__commemorative_husk, stone_land_use_rule__behavioral_competence).

% DUAL FORMULATION NOTE:
% The colloquial label 'the flood stone's rule' covers two structurally distinct claims and is decomposed per the epsilon-invariance principle. Sibling story stone_land_use_rule__behavioral_competence: the stone as a live prohibition — low epsilon, genuine coordination, beneficiaries without victims. This story: the stone as commemorative husk — the standing arrangement is the vacancy the inert stone decorates, epsilon 0.74, piton maintenance economics with parasitic capture by waterfront development. Same inscription, same stone, different epsilon: two files. The sibling reading is upstream (the founding state this reading measures drift from); this reading's vacancy is what the sibling's enforcement would have to overcome to re-form.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
