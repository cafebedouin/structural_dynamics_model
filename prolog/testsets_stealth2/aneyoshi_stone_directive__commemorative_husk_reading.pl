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
 *   constraint_id: aneyoshi_stone_directive__commemorative_husk_reading
 *   human_readable: Tsunami Warning Stones as Commemorative Husk (Inter-Catastrophe Interval Reading)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   Along the Sanriku coast, stone stelae raised after the 1896 and 1933
 *   tsunamis instruct descendants not to build below the marked line. The
 *   kernel 'aneyoshi_stone_directive' is read two ways. THIS story
 *   instantiates the commemorative_husk_reading: across the 78-year
 *   inter-catastrophe interval (1933-2011) the directive lost behavioral
 *   force almost everywhere — postwar generations built the flat land below
 *   the markers — and what persisted is a maintained memorial artifact whose
 *   operative demand is dead. The standing arrangement assessed here is the
 *   directive-as-institutional-form across that interval, judged by this
 *   reading's lights: it collected upkeep, ceremony, designation
 *   administration, and residual siting friction while delivering no
 *   verifiable protection, and its decay handed coastal land to development.
 *   KEY AGENTS (by structural relationship): - stone_directive_trustees:
 *   agenda-setter (organized/identity_locked) — administers the artifact, no
 *   longer the demand - coastal_development_interests: primary beneficiary
 *   (powerful/mobile) — gains legitimation cover and freed land from the
 *   lapse - heritage_preservation_institutions: secondary beneficiary
 *   (institutional/identity_locked) — mandate and budgets ride on the
 *   artifact - low_lying_coastal_residents: primary payer (moderate/trapped)
 *   — absorb the protection shortfall - municipal_taxpayers: payer
 *   (moderate/constrained) — fund maintenance of a dead function -
 *   descendant_hazard_zone_residents: excluded (powerless/trapped) — the
 *   inscriptions' addressees, absent from every allocation decision -
 *   disaster_research_community: analytical observer — sees the full
 *   compliance record The claim/metric gap is deliberate and load-bearing:
 *   the sibling behavioral_competence_reading will author the SAME referent
 *   as a low-extraction protective coordination; this reading authors high
 *   extraction and a decayed-function profile. Neither reading tunes to the
 *   engine; the corpus exists to measure their divergence.
 *
 * KEY AGENTS:
 *   - stone_directive_trustees: agenda_setter (organized/identity_locked) — curate and maintain the stones; custodianship is their inherited identity
 *   - coastal_development_interests: beneficiary with secondary payer position (powerful/mobile) — gained freed lowland and moral cover as enforcement faded; absorb occasional permit friction
 *   - heritage_preservation_institutions: beneficiary (institutional/identity_locked) — designation administration, subsidies, and institutional purpose anchored on the artifacts
 *   - memorial_tourism_operators: beneficiary (moderate/mobile) — modest seasonal receipts from tours and school trips
 *   - low_lying_coastal_residents: payer (moderate/trapped) — harbor-tied households holding the exposure the lapsed rule no longer mitigates
 *   - municipal_taxpayers: payer (moderate/constrained) — fund upkeep and ceremony for a function that no longer operates
 *   - descendant_hazard_zone_residents: excluded (powerless/trapped) — future occupants of zones opened during the lapse; consulted by no one
 *   - disaster_research_community: observer (analytical/analytical) — documents marker distributions, transmission chains, and differential outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, 0.7).
domain_priors:suppression_score(aneyoshi_stone_directive__commemorative_husk_reading, 0.45).
domain_priors:theater_ratio(aneyoshi_stone_directive__commemorative_husk_reading, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 0.8).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_directive__commemorative_husk_reading, "Tsunami Warning Stones as Commemorative Husk (Inter-Catastrophe Interval Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_directive__commemorative_husk_reading, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:requires_active_enforcement(aneyoshi_stone_directive__commemorative_husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__commemorative_husk_reading, '51ef83d1-79b7-4363-a58e-f7e0421af6cc').
narrative_ontology:cs_kernel_codification('51ef83d1-79b7-4363-a58e-f7e0421af6cc', fixed_text).
narrative_ontology:cs_authority_grounding('51ef83d1-79b7-4363-a58e-f7e0421af6cc', lineage).
narrative_ontology:cs_interpretation_layer_present('51ef83d1-79b7-4363-a58e-f7e0421af6cc').
narrative_ontology:cs_reading_relation('51ef83d1-79b7-4363-a58e-f7e0421af6cc', aneyoshi_stone_directive__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('51ef83d1-79b7-4363-a58e-f7e0421af6cc', foundational, inscription_force_requires_enforcement_continuity).
narrative_ontology:cs_axiom_status(inscription_force_requires_enforcement_continuity, holdable).
narrative_ontology:cs_axiom_grounding('51ef83d1-79b7-4363-a58e-f7e0421af6cc', inscription_force_requires_enforcement_continuity, empirically_contingent).
narrative_ontology:cs_axiom('51ef83d1-79b7-4363-a58e-f7e0421af6cc', secondary, artifact_survival_is_not_evidence_of_function).
narrative_ontology:cs_axiom_status(artifact_survival_is_not_evidence_of_function, holdable).
narrative_ontology:cs_axiom_grounding('51ef83d1-79b7-4363-a58e-f7e0421af6cc', artifact_survival_is_not_evidence_of_function, empirically_contingent).
narrative_ontology:cs_reference_frame('51ef83d1-79b7-4363-a58e-f7e0421af6cc', ancestral_warning_as_memorial).
narrative_ontology:cs_drift_state('51ef83d1-79b7-4363-a58e-f7e0421af6cc', post_2011_reevaluation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('51ef83d1-79b7-4363-a58e-f7e0421af6cc', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, heritage_preservation_institutions).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, memorial_tourism_operators).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, low_lying_coastal_residents).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, municipal_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Village preservation associations, temple custodians, and cultural-property committees along the Sanriku coast. They re-carve weathered characters, clear moss from the stones, register them as cultural properties, lead annual readings on disaster-anniversary dates, and answer school-group and press inquiries. Almost none of their current activity involves telling anyone where to build; their inherited office has become curatorship of the artifacts themselves, and stepping away from it would abandon an identity their families have held for generations.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, stone_directive_trustees, agenda_setter,
    organized, generational, identity_locked, regional).

% Land brokers, port contractors, fishery-infrastructure promoters, and municipal growth desks. As the living enforcement behind the old siting rule faded across the postwar decades, they purchased and filled lowland parcels that earlier generations had left empty, and they invoke the stones respectfully in promotional materials as proof of harmony with ancestral memory. Where a marker physically obstructs a site they petition to relocate it; where designation paperwork slows a project they absorb delay. Capital can move to inland or other-coast projects if returns disappoint.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests, beneficiary,
    powerful, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests, payer).

% Prefectural cultural-property boards, preservation societies, and local museums. They process designation filings, administer restoration subsidies, commission stonemasons, and publish interpretive materials. Their budgets, staffing rationales, and professional purposes are anchored on the continued existence and care of the artifacts; the organization has, in effect, become the care of the stones.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, heritage_preservation_institutions, beneficiary,
    institutional, generational, identity_locked, regional).

% Guesthouse owners, local guides, and excursion coordinators who fold the stones into regional-history tours and school trips. The receipts are modest and seasonal; a guide who lost the stones as a tour stop would re-route to shrines, harbors, or scenic capes without difficulty.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, memorial_tourism_operators, beneficiary,
    moderate, immediate, mobile, local).

% Fishing and small-trade households occupying the flat ground between the marker lines and the harbor. Housing is tied to the port, to family graves, and to inherited plots; elderly residents cannot realistically relocate. First postwar generations largely kept to the line at their elders' insistence; later generations treated the stones as scenery and heritage, buying affordable lowland lots. Whatever protection the stones once organized, these households absorbed the full consequence of its absence where compliance lapsed.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, low_lying_coastal_residents, payer,
    moderate, biographical, trapped, local).

% Ratepayers funding designation administration, restoration contracts, ceremony logistics, and signage through municipal budgets. They cannot opt out of the line items short of moving to another municipality, and no service they receive scales with what they pay.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, municipal_taxpayers, payer,
    moderate, biographical, constrained, regional).

% The future occupants of the zones opened during the enforcement lapse — the very people the inscriptions address ('raise your homes high, for your descendants'). They were absent from every meeting at which lowland parcels were platted, financed, and sold, inheriting siting decisions made before they existed and unable to exit the exposure those decisions fixed.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, descendant_hazard_zone_residents, excluded,
    powerless, generational, trapped, regional).

% Folklorists, seismologists, and disaster anthropologists who surveyed marker distributions mid-century, recorded oral transmission chains, and after 2011 compared outcomes across hamlets that obeyed the stones with those that did not. They bear none of the arrangement's costs and collect none of its flows; their seat exists to measure what happened.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, disaster_research_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_directive__commemorative_husk_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_directive__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: At founding, the arrangement converted episodic catastrophe memory into a persistent siting norm: carve the flood line into stone so that settlement discipline survives the generations who never saw the water. Across the interval it continued to coordinate one thing reliably — a communal calendar of remembrance (readings, upkeep, school instruction) — while the siting norm it was built to carry ceased being collectively enforced.
% TRANSFER_FUNCTION: Maintenance labor and municipal funds flow from households and ratepayers to custodial and heritage institutions; civic attention and children's instruction hours flow into the memorial form. Early in the interval the arrangement also transferred foregone lowland land-use from willing builders to collective safety; by interval end it transferred the appearance of protection to everyone while actual exposure remained concentrated on low-lying residents.
% ABSENT_VOICES: Hazard scientists had no seat: mid-century paleotsunami research quantified recurrence intervals that no custodial body ever used to revise the inscribed lines. The descendant residents of zones opened during the lapse had no seat at any platting decision. Villagers wanting markers relocated for development spoke only through informal petitions with no standing.
% DISAPPEARANCE_RATIONALE: If the stones, their upkeep, and the ceremony calendar vanished overnight, land-use outcomes would shift only marginally, because the directive's binding force was already gone — nobody's building decision currently waits on them. What would rearrange is the commemorative economy: custodial identities would lose their object, heritage programs and restoration budgets would dissolve, tourism routes and school curricula would lose a stop, and the annual remembrance rhythm that structures village calendars would end.
% FOUNDING_PROBLEM: After the 1896 and 1933 Sanriku tsunamis killed tens of thousands, coastal villages faced a transmission problem: the next inundation would arrive generations later, after everyone who remembered the water was dead. The stones were raised to solve it — fix the flood line in rock so descendants would not rebuild in the kill zone.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: paleotsunami sediment studies confirming the Jōgan 869 precedent and recurrence intervals, post-2011 official investigation commission findings, and recorded survivor testimony from the hamlet that kept to the line. These sources corroborate that the founding problem was real and geological; development-side actors and engineering agencies attest that the practical problem was superseded mid-interval by seawalls, hazard maps, and warning systems — hence the status is disputed rather than settled, and no beneficiary of the standing arrangement supplies the attestation.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__commemorative_husk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__commemorative_husk_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Claimed type is piton because the reading's core premise is functional atrophy: the directive was a working coordination at founding (fresh mass grave-memory, elders who compelled siting) whose enforcement dissolved across the interval, leaving an artifact maintained by inertia, designation law, and ceremony. Metrics are authored independently of that claim. Theater_ratio 0.80: by interval end the overwhelming majority of organized activity around the stones is commemorative (readings, restorations, signage, curricula) rather than land-use-steering. Extractiveness 0.70: the arrangement draws upkeep funds, administrative effort, residual siting friction, and inherited deference while, under this reading, delivering no validated protection — costs without the promised good is the definition of the husk. Suppression 0.45 is a composite and deliberately so (see the suppression_component_decomposition omega): behavioral enforcement of the demand decayed toward ~0.15 while legal self-protection of the artifacts (cultural-property designation that bars removal or relocation) grew toward ~0.7; the scalar nets them. Suppression is authored as a raw structural property and is not scaled by scope or directionality — only extraction is. Accessibility_collapse 0.25: the alternative to the directive (build the lowland) remained fully accessible and was exercised broadly; nothing collapsed. Resistance 0.60: sustained passive noncompliance across most sites, plus active relocation petitions and development pressure against obstructing markers. The three measurement series share one seven-point grid (t=0,13,26,39,52,65,78); no metric is missing from any row, so no scalar substitution occurs. The trajectories are monotonic — decay without oscillation — because the interval contained no intermediate catastrophe capable of resetting memory; the cycle-reset hypothesis belongs to the post-2011 era, outside this interval.
 *
 * PERSPECTIVAL GAP:
 *   From the trustee seat the arrangement is sacred stewardship — an unbroken chain of care that feels like the purest coordination, with no extraction perceptible from inside the liturgy. From the resident seat it is inherited background assurance that quietly failed to mean anything the day the family signed for a lowland plot. From the developer seat it began as an obstacle and ended as an ornament: useful precisely because it demands nothing anymore while lending moral texture to a brochure. The researcher seat sees the differential mortality record that none of the interior seats can see. The engine computes these per-seat classifications from the structural data; this story authors the structure and refuses to adjudicate which seat's experience is 'the' truth of the arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for coastal_development_interests (gains legitimation cover and freed land as the arrangement's enforcement fades — its benefit is real but derives partly from decay, which the secondary payer role partially offsets), heritage_preservation_institutions (mandate and budget), and memorial_tourism_operators (sliver receipts). Victim declarations drive high directionality for low_lying_coastal_residents — amplified toward the full-target end because their exit is trapped: housing welded to port, graves, and aging — and for municipal_taxpayers, constrained but less exposed. One directionality override is authored: power_atom 'organized' to d=0.5. The trustees occupy the organized atom alone in this story, and the derivation chain has no seat-class for pure administrators — agents who set and maintain an arrangement while collecting neither its rents nor bearing its costs; the structural data (no beneficiary listing, no victim listing, identity-locked custodianship) cannot yield them a d, so the symmetric value is declared explicitly. Development interests stay mobile, which damps their effective burden on the payer half of their dual position; residents stay trapped, which does the opposite for theirs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — transmit survival-critical siting knowledge across the memory gap — expired sometime in the postwar decades, when the last enforcers who had seen the water died and their successors inherited curatorship instead of command. Declaring mandatrophy_resolved true records that expiry. The classification does two kinds of preventive work. Against the romantic mislabel (these stones as timeless wisdom, a near-mountain of inherited prudence): the metrics show a construct requiring active maintenance, meeting substantial resistance, and suppressing nothing effectively — no natural law behaves that way. Against the cynical mislabel (a snare — elders knowingly farming descendants for ceremony budgets): no seat captures the operative gains; the extraction dissipates as deadweight upkeep and unpriced exposure, which is atrophy's signature, not design's. The piton cell (prohibitive-to-fix, diffuse gains) names the trap precisely: removal is culturally and legally barred, restoration of function would require rebuilding an enforcement culture that took 78 years to dissolve, and no fixer stands to gain enough to attempt either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitments,
    'This story is one reading of the aneyoshi_stone_directive kernel (commemorative_husk_reading): what structural content does the sibling behavioral_competence_reading assign, and where exactly is the disagreement located?',
    'Comparative settlement archaeology and archival enforcement records across Sanriku-coast sites: date building foundations below marker lines against the 1933-2011 interval to establish whether behavioral force persisted anywhere beyond isolated hamlets.',
    'The sibling reading assigns low extraction with protected-descendant beneficiaries on the same referent; this reading assigns high extraction with development-side gains. Whichever reading the evidence favors flips the classification of the shared arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitments, conceptual, 'Committer structure: one reading of a two-reading kernel; disagreement located in whether the directive retained behavioral force across the inter-catastrophe interval.').

omega_variable(
    site_heterogeneity_of_force,
    'Did the directive retain force at some sites (Aneyoshi-type hamlets with intact elder authority) while lapsing at others, such that both kernel readings are locally true of different places?',
    'Per-site compliance chronologies: dated elevation of postwar construction relative to each marker line, correlated with custodial-authority continuity at that site.',
    'If force is site-heterogeneous, the kernel decomposes further into site-specific constraints and no single reading governs the whole coast; this story''s epsilon becomes an interval-average rather than a uniform property.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(site_heterogeneity_of_force, empirical, 'Whether directive adherence varied systematically by site rather than failing uniformly.').

omega_variable(
    false_security_causation,
    'Did the maintained stones actively induce lowland settlement (a reassurance effect that raises attributable extraction), or did economic drivers act independently while the stones were merely irrelevant?',
    'Difference-in-differences on settlement density near maintained markers versus control shoreline stretches, before and after ceremonial institutionalization intensified.',
    'A demonstrated reassurance effect attributes lowland exposure to the standing arrangement and raises effective extraction on the resident seat; mere irrelevance concentrates this story''s extraction in deadweight maintenance cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_security_causation, empirical, 'Causal role of the memorial''s continued presence in opening the hazard zone to settlement.').

omega_variable(
    suppression_component_decomposition,
    'The net suppression scalar (0.45) composites two opposing components: behavioral enforcement of the siting demand decayed toward ~0.15 by interval end while legal self-protection of the artifacts (cultural-property designation barring removal or relocation) grew toward ~0.7. Which component dominates the arrangement''s operative coercion?',
    'Separate indices: counts of enforcement actions directed at builders versus designation shields invoked against removal or relocation petitions.',
    'Recomposition toward the enforcement component lowers suppression and hastens inertial fade; recomposition toward the shield component raises suppression and extends husk persistence indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_component_decomposition, empirical, 'Composition of measured suppression between decayed behavioral enforcement and growing artifact self-protection.').

omega_variable(
    counterfactual_protection_value,
    'Had the directive retained coast-wide behavioral force, would 2011-era casualties have fallen enough to substantiate the sibling reading''s protective claim?',
    'Inundation modeling of the 2011 event run against a counterfactual fully-compliant settlement pattern derived from marker-line elevations.',
    'Strengthens or weakens the sibling reading''s vindication case; does not alter this story''s epsilon, whose referent is the standing arrangement as it actually operated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_protection_value, empirical, 'Counterfactual casualty reduction under coast-wide compliance, bearing on the sibling reading''s claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__commemorative_husk_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(aney_tr_t13, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 13, 0.3).
narrative_ontology:measurement(aney_tr_t26, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 26, 0.45).
narrative_ontology:measurement(aney_tr_t39, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 39, 0.58).
narrative_ontology:measurement(aney_tr_t52, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 52, 0.68).
narrative_ontology:measurement(aney_tr_t65, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 65, 0.76).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 78, 0.8).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(aney_be_t13, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 13, 0.42).
narrative_ontology:measurement(aney_be_t26, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 26, 0.5).
narrative_ontology:measurement(aney_be_t39, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 39, 0.56).
narrative_ontology:measurement(aney_be_t52, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 52, 0.62).
narrative_ontology:measurement(aney_be_t65, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 65, 0.67).
narrative_ontology:measurement(aney_be_t78, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 78, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t0, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(aney_su_t13, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 13, 0.56).
narrative_ontology:measurement(aney_su_t26, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 26, 0.52).
narrative_ontology:measurement(aney_su_t39, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 39, 0.49).
narrative_ontology:measurement(aney_su_t52, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 52, 0.47).
narrative_ontology:measurement(aney_su_t65, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 65, 0.46).
narrative_ontology:measurement(aney_su_t78, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 78, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__commemorative_husk_reading, identity_coordination).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive__behavioral_competence_reading).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__commemorative_husk_reading, sanriku_post2011_reconstruction_zoning).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the Aneyoshi tsunami stones.' The label conflates two structurally distinct claims about one standing arrangement: (a) the directive retained binding land-use force across the 1933-2011 interval without validation — the sibling behavioral_competence_reading, low epsilon, protective coordination delivered; (b) the directive lost behavioral force during the interval and survives as maintained memorial — this story, high epsilon, costs collected without validated protection. Per the epsilon-invariance principle these are written as two files with separate epsilon, beneficiary structures, and claimed types, linked here. The upstream/downstream edge to sanriku_post2011_reconstruction_zoning records that whatever the interval's true reading, the 2011 outcome became an input to mandatory-zone redesignation, seawall siting, and elevation-subsidy design.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(aneyoshi_stone_directive__commemorative_husk_reading, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
