% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__commemorative_husk_reading, []).

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
 *   constraint_id: aneyoshi_stone_directive__commemorative_husk_reading
 *   human_readable: Aneyoshi Stone Directive — Commemorative Husk Reading
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   On the Sanriku coast, stone markers erected by tsunami survivors instruct
 *   descendants not to build below the inundation reach. At Aneyoshi the
 *   markers stand between the 1933 Shōwa Sanriku tsunami and the 2011 Tōhoku
 *   tsunami — a 78-year interval in which no major wave tested the
 *   injunction. This story instantiates the commemorative_husk_reading of
 *   that arrangement: across the windless decades the directive's behavioral
 *   force lapsed, the markers persisted as memorial artifacts maintained by
 *   ritual and repainting, and the post-2011 revival enforces setbacks in the
 *   markers' name on the strength of a mythologized continuity. The standing
 *   arrangement under contest — the stone-directive land-use order across the
 *   whole interval — is assessed by this reading's own lights: it now
 *   suppresses economically rational coastal development on an authority
 *   whose continuous-behavioral pedigree is false, while the decay decades
 *   transferred windfall land value to development interests at the expense
 *   of the settlers who later bore the wave. CONSTRAINT FAMILY NOTE: the
 *   aneyoshi_stone_directive kernel decomposes into two epsilon-invariant
 *   readings sharing one referent. The sibling file,
 *   behavioral_competence_reading, authors low epsilon over the same
 *   arrangement (continuous binding coordination, rope-like profile) and is
 *   upstream in legitimacy terms — its continuity claim is the evidentiary
 *   warrant the post-2011 enforcement cites. This file authors high epsilon
 *   because the behavioral record it reads shows lapse, not obedience. The
 *   two files are linked via network.affects_constraints; neither hedges
 *   across the other.
 *
 * KEY AGENTS:
 *   - municipal_planning_authority: agenda-setting administrator (institutional/constrained) — codifies rebuilding setbacks citing the markers' authority; collects grant eligibility and political credit
 *   - elders_custodians: customary administrator (moderate/identity_locked) — maintain, repaint, and interpret the markers; fused with the custodial role
 *   - coastal_development_interests: dual-positioned actor (organized/mobile) — captured windfall seaward development during the unenforced decades; now faces revived setback enforcement
 *   - disaster_memory_institutions: beneficiary (organized/mobile) — heritage boards, museums, and remembrance programming funded on the markers' prominence
 *   - coastal_parcel_owners: primary payer (moderate/trapped) — hold seaward parcels whose building and rebuilding rights are curtailed
 *   - prospective_coastal_settlers: payer and excluded voice (powerless/mobile) — would-be residents barred from both the land and the planning conversation
 *   - gap_generation_settlers: historical payer (powerless/trapped) — settled seaward during the enforcement lapse and absorbed the 2011 losses
 *   - risk_engineering_assessors: excluded expert seat (institutional/constrained) — instrumented hazard mapping crowded out by the markers' moral authority
 *   - academic_analyst: analytical observer (analytical/analytical) — compares archival, oral, and physical evidence across both readings of the record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, 0.72).
domain_priors:suppression_score(aneyoshi_stone_directive__commemorative_husk_reading, 0.6).
domain_priors:theater_ratio(aneyoshi_stone_directive__commemorative_husk_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__commemorative_husk_reading, tangled_rope).
narrative_ontology:human_readable(aneyoshi_stone_directive__commemorative_husk_reading, "Aneyoshi Stone Directive — Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_stone_directive__commemorative_husk_reading, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:requires_active_enforcement(aneyoshi_stone_directive__commemorative_husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__commemorative_husk_reading, 'ceb899c7-0b85-4a0b-a2c3-fab7ec16d003').
narrative_ontology:cs_kernel_codification('ceb899c7-0b85-4a0b-a2c3-fab7ec16d003', fixed_text).
narrative_ontology:cs_authority_grounding('ceb899c7-0b85-4a0b-a2c3-fab7ec16d003', lineage).
narrative_ontology:cs_interpretation_layer_present('ceb899c7-0b85-4a0b-a2c3-fab7ec16d003').
narrative_ontology:cs_reading_relation('ceb899c7-0b85-4a0b-a2c3-fab7ec16d003', aneyoshi_stone_directive__behavioral_competence_reading, forecloses).
narrative_ontology:cs_axiom('ceb899c7-0b85-4a0b-a2c3-fab7ec16d003', foundational, commemoration_is_not_compliance).
narrative_ontology:cs_axiom_status(commemoration_is_not_compliance, holdable).
narrative_ontology:cs_axiom_grounding('ceb899c7-0b85-4a0b-a2c3-fab7ec16d003', commemoration_is_not_compliance, empirically_contingent).
narrative_ontology:cs_axiom('ceb899c7-0b85-4a0b-a2c3-fab7ec16d003', foundational, binding_force_requires_lived_transmission).
narrative_ontology:cs_axiom_status(binding_force_requires_lived_transmission, holdable).
narrative_ontology:cs_axiom_grounding('ceb899c7-0b85-4a0b-a2c3-fab7ec16d003', binding_force_requires_lived_transmission, empirically_contingent).
narrative_ontology:cs_reference_frame('ceb899c7-0b85-4a0b-a2c3-fab7ec16d003', ancestral_testamentary_command).
narrative_ontology:cs_drift_state('ceb899c7-0b85-4a0b-a2c3-fab7ec16d003', post_2011_revival, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('ceb899c7-0b85-4a0b-a2c3-fab7ec16d003', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, disaster_memory_institutions).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, municipal_planning_authority).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, coastal_parcel_owners).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, prospective_coastal_settlers).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, gap_generation_settlers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, elders_custodians).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers post-disaster rebuilding standards and setback zoning for the coastal district, citing the stone markers' authority in ordinances and public communications. Receives reconstruction grants tied to hazard-mitigation compliance and gains electoral credit for honoring ancestral warnings. It is legally free to rest zoning solely on modern inundation modeling, but dropping the markers' citation would provoke cultural backlash it cannot absorb.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, municipal_planning_authority, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__commemorative_husk_reading, municipal_planning_authority, beneficiary).

% Repaint, repair, and preside over the stone markers; teach children the inscription's meaning and lead anniversary observances. Their standing in the village rests on the custodial role itself — setting it aside would unravel their social identity and the community's felt continuity with the 1933 survivors. They interpret which parcels the injunction reaches and how far seaward it runs.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, elders_custodians, agenda_setter,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__commemorative_husk_reading, elders_custodians, beneficiary).

% Developers, guesthouse operators, and land aggregators who financed seaward projects during the decades when the customary prohibition went unenforced, acquiring hazard-exposed land cheaply. After 2011 they face setback rules invoked in the markers' name. Capital can relocate to other coastlines, but sunk local holdings, permits, and half-built projects bind their current positions.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests, payer).

% Prefectural heritage boards, disaster museums, and school-program providers whose budgets, exhibits, and visitor flows depend on the markers' prominence as symbols of vernacular disaster wisdom. Their annual funding cases cite the markers; their portfolios could re-anchor on other heritage assets if the markers' stature faded.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, disaster_memory_institutions, beneficiary,
    organized, generational, mobile, national).

% Households holding title to parcels seaward of or near the marker line. Rebuilding and expansion rights are curtailed; the land is illiquid because any buyer faces the same limits; family graves, workplaces, and fishing berths tie them to the plots regardless.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, coastal_parcel_owners, payer,
    moderate, biographical, trapped, local).

% Younger families and in-migrants who would build or buy near the shore for fishing livelihoods and comparatively affordable land. Setback rules price or bar them out, and no seat in the planning process represents their interest; their recourse is to settle elsewhere.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, prospective_coastal_settlers, payer,
    powerless, immediate, mobile, regional).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__commemorative_husk_reading, prospective_coastal_settlers, excluded).

% The cohort that established homes and businesses seaward of the markers between the mid-century decades and 2011, when no active enforcement signaled danger. They absorbed the tsunami's losses directly; survivors and heirs now live under the revived rules that their presence made politically possible.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, gap_generation_settlers, payer,
    powerless, biographical, trapped, local).

% Engineers and geoscientists producing instrumented inundation maps, seawall specifications, and probabilistic risk pricing. Their proposals compete for budget and authority, but the markers' moral weight frames setbacks as already settled, leaving their recommendations advisory and their alternatives underfunded.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, risk_engineering_assessors, excluded,
    institutional, generational, constrained, regional).

% Researchers in disaster anthropology and memory studies who document the markers' history and test competing accounts of whether the prohibition guided siting decisions across the windless decades. Positioned outside local obligations, they can weigh archival, oral, and physical evidence against both readings of the record.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, academic_analyst, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_directive__commemorative_husk_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_directive__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps dwellings and critical assets above the reach of rare, generation-spanning tsunami inundation, addressing the fact that individual builders systematically underprice catastrophes no living person has witnessed.
% TRANSFER_FUNCTION: Moves siting freedom and developable land value away from seaward parcels — from parcel owners and would-be settlers toward a collective safety margin — and, during the unenforced decades, moved windfall land value toward development interests; post-2011 it channels grants, remembrance funding, and political credit toward the municipal authority and memory institutions.
% ABSENT_VOICES: Risk engineers with instrumented alternatives, economists proposing priced risk, and would-be seaward settlers are outside the planning conversation; the dead of the gap generation, whose siting choices would settle the kernel dispute, cannot testify. Dissent lacks a forum because the markers' moral authority frames setbacks as filial duty rather than negotiable policy.
% DISAPPEARANCE_RATIONALE: If the marker-based arrangement vanished overnight, coastal zoning would reorganize around instrumented inundation mapping and engineered defenses, seaward land values would shift as building rights repriced, the remembrance funding stream would lose its anchor, and the village's civic rituals would lose their central object.
% FOUNDING_PROBLEM: After the 1896 and 1933 Sanriku tsunamis killed tens of thousands along this coast, survivors carved injunctions into stone markers at the inundation's reach — do not build your homes below this point — so that descendants who would never see such a sea would nonetheless site their lives above it.
% FOUNDING_PROBLEM_CORROBORATION: Seismological and engineering literature — Sanriku tsunami surveys and 2011 inundation mapping showing the run-up at Aneyoshi reached approximately the marker line — together with Miyako City archival records of the markers' erection, attest the founding hazard and the transmission problem from outside the arrangement's beneficiary set. No source outside the dispute attests that the injunction continuously governed siting decisions between 1933 and 2011; that continuity is precisely what the two readings contest.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__commemorative_husk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__commemorative_husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__commemorative_husk_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_directive__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.72 for the standing arrangement as this reading assesses it: the revived order binds land-use decisions through an authority whose behavioral continuity is asserted rather than real, suppressing development that a transparent risk-priced regime would treat differently, and the decay decades layered a windfall transfer onto the same structure. Suppression is authored at 0.6 as a raw structural property — customary sanction, municipal codification, and small-community reputational pressure — and is deliberately NOT scaled by power or scope; only extractiveness is scaled downstream by the engine. Theater_ratio at 0.62 reflects this reading's core claim: across the windless decades maintenance was overwhelmingly commemorative (repainting, anniversaries, school visits) while siting content went unenforced, and post-2011 citation leans on mythologized continuity. Accessibility_collapse sits at 0.35 because alternatives — instrumented inundation mapping, engineered defenses, priced risk, relocation subsidies — remain available though culturally crowded out. Resistance at 0.45 reflects developer and landowner contestation muted by filial framing. The temporal series share one grid (t=0..90, nine points, all three metrics at every point): extractiveness accumulates monotonically (T17 will read the rising base_extractiveness as an abductive hypothesis, not a reclassification), theater climbs through the windless decades and dips slightly after 2011 as citational function partially revives, and suppression_requirement traces the story's spine — enforcement decay to near zero by t=72, then a sharp post-catastrophe ratchet. Coalition note: parcel owners and prospective settlers occupy complementary positions (holders and entrants) and could in principle contest jointly, but filial framing and illiquidity have so far fragmented them.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the elders' custodial seat the arrangement is sacred continuity — the markers are the ancestors speaking, and questioning enforcement is impiety. From the development-interests seat the same structure is arbitrary confiscation invoked selectively after the risk has already materialized. From the parcel-owner seat it is an inherited burden on illiquid family land. From the memory-institution seat it is a funding anchor. Same-level differentiation is sharpest among moderate-power actors: elders (identity_locked — exit would dissolve their social selves), parcel owners (trapped — land and graves immobile), and memory institutions (mobile — portfolios could re-anchor on other heritage) hold comparable standing yet experience wholly different constraints because their exits differ. The engine computes these per-seat classifications from the structural data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Disaster memory institutions and the municipal planning authority derive directionality near the beneficiary end: they collect funding, political credit, and administrative warrant from the arrangement's operation. Coastal development interests are genuinely dual-positioned — the decay decades paid them windfall land value (beneficiary side), while the post-2011 revival suppresses their projects (payer side); their secondary_role records this and their net directionality sits mid-range rather than at either pole. Coastal parcel owners and gap-generation settlers derive near the full-target end: they bear curtailed rights and, in the gap generation's case, catastrophic losses that the lapse failed to signal. Prospective settlers are targets with mobile exit — the suppression works by keeping them out rather than holding them in. Risk engineering assessors are excluded rather than coordinated: their instrumented alternatives are sidelined by the same moral authority the enforcement cites, which is part of the suppression picture but not a beneficiary/victim declaration.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — carry rare-event hazard knowledge to descendants who will not witness the event — outlived its mechanism during the windless decades: the artifact persisted while the behavioral function lapsed, which is why mandatrophy_resolved is declared true. The post-2011 revival re-tasked the husk rather than restoring the original mechanism (lived transmission is gone; what enforces now is codification plus myth). The tangled-rope classification prevents two opposite mislabels: a pure-snare reading would erase the genuine coordination content — the setback principle the markers encode matched the 2011 run-up, and keeping settlement above it saves lives — while a pure-rope reading would erase the asymmetric extraction: myth-grounded enforcement, the decay-decade windfall, and the suppression of instrumented alternatives. A piton label tempts (high theater, diffuse current gains) but fails the cost-asymmetry test: development interests captured concentrated value during the decay phase, and enforcement is actively administered, not inertially maintained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the commemorative_husk_reading of the aneyoshi_stone_directive kernel: the stone-carved prohibition treated as a land-use arrangement whose behavioral force lapsed during the 78-year inter-catastrophe interval. The sibling reading, behavioral_competence_reading (separate file), holds the directive retained binding force across the same interval. The entire kernel contest is located in the behavioral record of siting decisions between 1933 and 2011.',
    'Archival building-permit and cadastral analysis for parcels seaward of the marker line, 1933-2011, cross-checked against oral-history collections and aerial photograph chronology.',
    'If the sibling reading prevails, epsilon collapses toward the coordination-cost floor, the beneficiary structure inverts (no decay windfall to development interests), and the type shifts toward rope. If this reading prevails, the high-epsilon profile stands and the post-2011 enforcement loses the evidentiary warrant it cites.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Kernel-reading commitment: husk versus behavioral-competence account of the same stone directive.').

omega_variable(
    gap_period_siting_record,
    'What did actual siting decisions show between 1933 and 2011 — did any construction occur seaward of the marker line, and under what permission or silence?',
    'Miyako City permit archives, cadastral overlays of the marker line, and dated aerial photography of the hamlet''s built footprint across the windless decades.',
    'Documented seaward construction confirms the husk reading and dates the behavioral collapse; a clean record seaward of the line would support the sibling reading and force re-authoring of this file''s epsilon and victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gap_period_siting_record, empirical, 'Whether the inter-catastrophe behavioral record shows compliance, lapse, or mixed drift.').

omega_variable(
    counterfactual_protection_credit,
    'Did observance of the marker line cause Aneyoshi''s survival in 2011, or would topography, hamlet size, and the 1933 survivors'' direct siting choices have produced the same outcome without any continuing normative force?',
    'Comparative inundation modeling of the 2011 event with and without compliance-era siting, benchmarked against neighboring hamlets with similar topography and no marker tradition.',
    'If topography and founding-generation siting suffice, the revived arrangement''s coordination claim is largely credited to a dead mechanism and the current profile drifts toward snare; if ongoing observance materially narrowed exposure, the coordination half of the tangled-rope reading is secured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_protection_credit, empirical, 'Causal credit for 2011 survival: living norm versus inherited geography and founding-generation choices.').

omega_variable(
    revival_enforcement_durability,
    'Will the post-2011 enforcement intensity persist as windless decades accumulate, or will it re-decay along the same trajectory as the 1933-2011 interval?',
    'Longitudinal tracking of setback-variance requests, permit outcomes, and ritual-maintenance budgets over the coming thirty years.',
    'Re-decay would confirm a cyclical husk pattern (catastrophe-driven ratchet, inter-catastrophe lapse) and imply the measured high epsilon is phase-dependent; durable enforcement would establish a structurally new arrangement distinct from the pre-2011 husk.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revival_enforcement_durability, empirical, 'Durability of the post-2011 enforcement ratchet across the next windless interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__commemorative_husk_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_husk_tr_t0, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(aneyoshi_husk_tr_t12, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(aneyoshi_husk_tr_t24, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(aneyoshi_husk_tr_t36, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 36, 0.49).
narrative_ontology:measurement(aneyoshi_husk_tr_t48, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 48, 0.57).
narrative_ontology:measurement(aneyoshi_husk_tr_t60, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 60, 0.64).
narrative_ontology:measurement(aneyoshi_husk_tr_t72, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 72, 0.71).
narrative_ontology:measurement(aneyoshi_husk_tr_t80, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 80, 0.67).
narrative_ontology:measurement(aneyoshi_husk_tr_t90, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 90, 0.62).

% Extraction over time
narrative_ontology:measurement(aneyoshi_husk_be_t0, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(aneyoshi_husk_be_t12, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 12, 0.21).
narrative_ontology:measurement(aneyoshi_husk_be_t24, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 24, 0.26).
narrative_ontology:measurement(aneyoshi_husk_be_t36, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 36, 0.31).
narrative_ontology:measurement(aneyoshi_husk_be_t48, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 48, 0.37).
narrative_ontology:measurement(aneyoshi_husk_be_t60, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 60, 0.44).
narrative_ontology:measurement(aneyoshi_husk_be_t72, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 72, 0.52).
narrative_ontology:measurement(aneyoshi_husk_be_t80, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 80, 0.63).
narrative_ontology:measurement(aneyoshi_husk_be_t90, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 90, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_husk_su_t0, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(aneyoshi_husk_su_t12, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 12, 0.2).
narrative_ontology:measurement(aneyoshi_husk_su_t24, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 24, 0.16).
narrative_ontology:measurement(aneyoshi_husk_su_t36, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 36, 0.13).
narrative_ontology:measurement(aneyoshi_husk_su_t48, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 48, 0.11).
narrative_ontology:measurement(aneyoshi_husk_su_t60, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 60, 0.1).
narrative_ontology:measurement(aneyoshi_husk_su_t72, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 72, 0.09).
narrative_ontology:measurement(aneyoshi_husk_su_t80, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 80, 0.42).
narrative_ontology:measurement(aneyoshi_husk_su_t90, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 90, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__commemorative_husk_reading, resource_allocation).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__commemorative_husk_reading, behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the aneyoshi_stone_directive kernel decomposes into two epsilon-invariant readings sharing one referent (the standing stone-directive land-use arrangement, 1933-present). behavioral_competence_reading authors low epsilon — continuous binding coordination, rope-like profile. This file, commemorative_husk_reading, authors high epsilon — behavioral lapse across the windless decades, commemorative maintenance, and a post-2011 revival enforcing setbacks on mythologized continuity. The sibling is upstream in legitimacy terms: its continuity claim supplies the evidentiary warrant the post-2011 enforcement cites, and this reading attacks that warrant. Each file links the other via network.affects_constraints; neither averages across the contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
