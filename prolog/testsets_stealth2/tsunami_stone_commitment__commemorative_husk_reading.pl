% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__commemorative_husk_reading, []).

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
 *   constraint_id: tsunami_stone_commitment__commemorative_husk_reading
 *   human_readable: Tsunami Stone Commitment — Commemorative Husk Reading
 *   domain: disaster_anthropology/commitment_systems/institutional_memory
 *
 * SUMMARY:
 *   After the 1896 Meiji Sanriku tsunami (~22,000 dead) and the 1933 Showa
 *   Sanriku tsunami, coastal villages carved stone stelae marking the reach
 *   of the water with instructions not to build or dwell below them. This
 *   story authors ONE reading of that commitment — the
 *   commemorative_husk_reading: by the late twentieth century the stelae
 *   functioned as commemorative heritage, not as binding siting rules;
 *   observed compliance (most famously the survival of the village that had
 *   moved uphill after the earlier waves) is attributed under this reading to
 *   confounders — geographic isolation, economic marginality, post-disaster
 *   relocation subsidies — rather than to enforcement flowing from the
 *   inscriptions. The standing arrangement this story is about, and the sole
 *   referent of epsilon, is the husk arrangement itself: stones cleaned,
 *   ceremonialized, and toured as heritage while the coast below the carved
 *   lines was rebuilt and developed behind seawalls. Under this reading the
 *   arrangement transfers tsunami exposure from the present to future
 *   residents — the gains (hazard-zone land value, tax base, memorial
 *   tourism) accrue now, the costs arrive later and catastrophically. Per the
 *   epsilon-invariance principle this file is a separate constraint from its
 *   siblings (behavioral_competence_reading, catastrophe_validation_axis);
 *   the three are linked in network.affects_constraints and the contest
 *   between them is carried in omega variables, never averaged into this
 *   file's numbers. KEY AGENTS (by structural relationship): -
 *   coastal_municipal_governments: agenda-setting administrator
 *   (institutional/constrained) — maintains commemoration, could redraw the
 *   building line, captures tax base - coastal_development_interests: primary
 *   beneficiary (powerful/mobile) — captures hazard-zone land value at prices
 *   that price in no recurrence - local_memorial_associations: secondary
 *   beneficiary and commemoration administrator (organized/identity_locked) —
 *   custodians of the stones' meaning - future_coastal_residents: primary
 *   target (powerless/trapped) — inherits unmitigated exposure; absent from
 *   every decision that priced it - stone_lineage_elders: excluded
 *   counter-voice (powerless/identity_locked) — holds the injunction reading
 *   of the stones, holds no seat - tsunami_disaster_researchers: analytical
 *   observer (organized/analytical) — documents the decay every reading draws
 *   on
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__commemorative_husk_reading, 0.72).
domain_priors:suppression_score(tsunami_stone_commitment__commemorative_husk_reading, 0.3).
domain_priors:theater_ratio(tsunami_stone_commitment__commemorative_husk_reading, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(tsunami_stone_commitment__commemorative_husk_reading, "Tsunami Stone Commitment — Commemorative Husk Reading").
narrative_ontology:topic_domain(tsunami_stone_commitment__commemorative_husk_reading, "disaster_anthropology/commitment_systems/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__commemorative_husk_reading, '7cfd9de3-5d87-4548-8075-8bb254fd84b3').
narrative_ontology:cs_kernel_codification('7cfd9de3-5d87-4548-8075-8bb254fd84b3', fixed_text).
narrative_ontology:cs_authority_grounding('7cfd9de3-5d87-4548-8075-8bb254fd84b3', lineage).
narrative_ontology:cs_interpretation_layer_present('7cfd9de3-5d87-4548-8075-8bb254fd84b3').
narrative_ontology:cs_reading_relation('7cfd9de3-5d87-4548-8075-8bb254fd84b3', tsunami_stone_commitment__behavioral_competence_reading, forecloses).
narrative_ontology:cs_reading_relation('7cfd9de3-5d87-4548-8075-8bb254fd84b3', tsunami_stone_commitment__catastrophe_validation_axis, coexists_with).
narrative_ontology:cs_axiom('7cfd9de3-5d87-4548-8075-8bb254fd84b3', foundational, inscription_symbolic_not_binding).
narrative_ontology:cs_axiom_status(inscription_symbolic_not_binding, holdable).
narrative_ontology:cs_axiom_grounding('7cfd9de3-5d87-4548-8075-8bb254fd84b3', inscription_symbolic_not_binding, empirically_contingent).
narrative_ontology:cs_axiom('7cfd9de3-5d87-4548-8075-8bb254fd84b3', foundational, compliance_attributable_to_confounders).
narrative_ontology:cs_axiom_status(compliance_attributable_to_confounders, holdable).
narrative_ontology:cs_axiom_grounding('7cfd9de3-5d87-4548-8075-8bb254fd84b3', compliance_attributable_to_confounders, empirically_contingent).
narrative_ontology:cs_reference_frame('7cfd9de3-5d87-4548-8075-8bb254fd84b3', commemorative_monument_frame).
narrative_ontology:cs_drift_state('7cfd9de3-5d87-4548-8075-8bb254fd84b3', post_2011_tohoku_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('7cfd9de3-5d87-4548-8075-8bb254fd84b3', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__commemorative_husk_reading, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__commemorative_husk_reading, local_memorial_associations).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__commemorative_husk_reading, coastal_municipal_governments).
narrative_ontology:constraint_victim(tsunami_stone_commitment__commemorative_husk_reading, future_coastal_residents).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__commemorative_husk_reading, seawall_substitution_doctrine).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__commemorative_husk_reading, commemorative_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer land-use approval, seawall construction, and the memorial calendar for the Sanriku coastal towns. They maintain the stones as heritage objects — cleaning, plaques, annual ceremonies — while approving reconstruction and new construction on low-lying shoreline behind seawalls. They collect property tax and fisheries-processor revenue from hazard-zone parcels. They could draw the carved inundation lines as binding building lines but have not, because relocation would shrink the tax base, contradict seawall investment, and cost electoral support; they hold the deferred liability when the water returns.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, coastal_municipal_governments, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__commemorative_husk_reading, coastal_municipal_governments, beneficiary).

% Build and operate on the low-lying coast — fisheries processing, port works, seaside lodging, housing — at land prices that do not price in tsunami recurrence. The stones' presence as revered monuments signals a community at peace with its hazard history rather than a legal bar to building. Land value and operating revenue flow to them; when the water returns, a share of their losses is covered by national reconstruction subsidies.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, coastal_development_interests, beneficiary,
    powerful, immediate, mobile, regional).

% Village associations and heritage volunteers who clean the stones, organize the annual reading of names, and run school visits. Their standing in the community rests on custodianship of the stones' meaning; they translate the carved instructions into stories of remembrance rather than rules for where building may occur. Stepping away from the custodianship would dissolve the practice that constitutes the association.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, local_memorial_associations, beneficiary,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__commemorative_husk_reading, local_memorial_associations, agenda_setter).

% Successive generations who inhabit the coast after the siting decisions have been made — culminating in the households living below the carved inundation lines in 2011. They inherit exposure they did not choose: seawalls look like protection, ceremonies look like preparedness, and the stones read as history rather than as a boundary. Nothing about their position lets them decline the exposure before the event; afterward it is paid in lives and destroyed towns.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, future_coastal_residents, payer,
    powerless, generational, trapped, regional).

% Descendants of 1896 and 1933 survivors who carry the oral instruction that the stones mark where the water reached and that dwelling below them invites the wave. They hold the counter-reading of what the stones say but hold no seat on land-use boards, heritage committees, or reconstruction councils; at ceremonies their statements are received as sentiment, and in planning offices as folklore.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, stone_lineage_elders, excluded,
    powerless, generational, identity_locked, local).

% Field researchers who survey the stones, map carved inundation heights against settlement footprints, and publish on why written warnings decay. Before 2011 their cautions about seawall-induced risk-taking sat outside municipal planning; after 2011 their archives became the shared evidentiary base every interpretation of the stones draws on.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, tsunami_disaster_researchers, observer,
    organized, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tsunami_stone_commitment__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:fixing_cost_class(tsunami_stone_commitment__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The commemorative arrangement coordinates collective memory of the 1896 and 1933 tsunamis: annual ceremonies, stone maintenance, school instruction, and heritage tourism give the coast a shared ritual of remembrance. Under this reading it no longer coordinates land-use restraint — that function is not performed.
% TRANSFER_FUNCTION: Transfers tsunami exposure from the present to future coastal residents: hazard-zone land is developed and taxed now, and the cost of the unheeded warning is deferred to whoever inhabits the coast when the wave recurs. Land value, tax base, and memorial tourism revenue move to development actors, municipalities, and custodian associations; risk moves to the unborn and the unrepresented.
% ABSENT_VOICES: The descendants of 1896 and 1933 survivors who carried oral knowledge of the stones as injunctions had no seat in land-use, heritage, or reconstruction decisions; disaster researchers warning about seawall-induced risk-taking sat outside municipal planning; and the class that ultimately pays — future residents — is structurally absent because it does not yet exist and holds no franchise. Unanimity around the commemorative reading arose in a room where none of these seats were filled.
% DISAPPEARANCE_RATIONALE: If the husk arrangement vanished overnight — stones removed, ceremonies ended — the commemorative economy would reorganize: memorial tourism, the municipal identity calendar, school programming, and the custodian associations all depend on the stones as objects of practice. The risk structure, by contrast, would not improve at all, because under this reading the arrangement performs no protection; its removal costs the coast nothing in safety and something in meaning, which is the reading's central point.
% FOUNDING_PROBLEM: After the 1896 Meiji Sanriku tsunami (~22,000 dead) and the 1933 Showa Sanriku tsunami, coastal villages erected stone stelae marking the water's reach with instructions not to build or dwell below them; the founding problem was permanent land-use restraint inside the inundation zone.
% FOUNDING_PROBLEM_CORROBORATION: The founding hazard is corroborated from outside every benefiting party by the physical record itself: the 2011 Tohoku inundation exceeded the carved lines and the casualty record (~19,000 dead) attests the problem's persistence without any party's self-attestation. Disaster-research literature and the oral testimony of survivors' descendants corroborate that the stelae were erected as siting instructions, not as monuments. No corroboration depends on the development interests, municipalities, or custodian associations that benefit from the husk arrangement.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__commemorative_husk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tsunami_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__commemorative_husk_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tsunami_stone_commitment__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tsunami_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.72) because the arrangement's operation moves catastrophic risk onto a class that holds no seat while land value and tax base accrue immediately to parties that do; the extraction is omission-shaped (non-protection) rather than rent-shaped, but under this reading the omission is the arrangement's product, not its accident. Suppression is authored low-moderate (0.3) and is raw and unscaled per the framework — no machinery coerces compliance or punishes dissent; the arrangement's hold on caution works through framing (seawalls that look like protection, ceremonies that look like preparedness) and through structural exclusion of the counter-voice. Theater_ratio is high (0.75): the dominant activity the stones now organize is commemorative performance — cleaning, plaques, annual name-readings, school visits — against a protective function this reading scores near zero. accessibility_collapse is low (0.2): understanding the stones as husks closes no alternatives; building on the coast remains fully available and even legitimated by the remembrance framing. resistance is moderate-low (0.3): elders' oral warnings, researcher cautions, and post-2011 relocation advocacy were real but marginal before the event. Claim and metrics are independent: the claimed type is what this reading takes to be structurally true (a degraded remnant persisting by inertia and performance, with no seat administering it for profit), and the metrics are what the record descriptively shows; the engine's computed type may diverge and that divergence is data. The temporal series run on one shared grid (1933, 1950, 1965, 1980, 1995, 2011) with every tracked metric authored at every point. suppression_requirement is tracked because the story's narrative is precisely an enforcement-capacity collapse: active enforcement force decays monotonically from 0.55 to 0.30, and the residual 0.30 is the framing/exclusion layer that base_properties.suppression measures at end-state — enforcement machinery rotted while the framing layer that needs no machinery persisted.
 *
 * PERSPECTIVAL GAP:
 *   The divergence is extreme because the paying seat is temporally absent. From the agenda-setting municipal seat the arrangement is heritage stewardship plus revenue: the stones are an asset, the seawalls are protection, the tax base is the coast's livelihood — no extraction is perceptible from inside it. From the development seat the arrangement is opportunity: land that prices in no recurrence. From the future-resident seat there is no perception at all before the event — the seat cannot observe the arrangement that prices its exposure — and after the event the perception arrives as casualty counts. Coalition power is unavailable to the target class in the usual sense: its members are dispersed across generations and cannot coordinate before they exist. The engine computes per-seat classifications from the structural data; the structural prediction is that the agenda-setter seat computes mild net benefit, the development seat strong net benefit, and the future-resident seat maximal effective extraction — among the widest perspectival gaps a corpus seat can carry, because one of the parties does not yet exist to disagree.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation. coastal_development_interests and local_memorial_associations sit near the beneficiary end (low d): development interests with mobile capital and immediate horizons sit nearest the subsidized end; memorial associations collect meaning and tourism but administer only the commemorative layer. future_coastal_residents sit at the full-target end (d near 1.0): they bear the entire deferred cost, are powerless, and are trapped — their exit is unavailable before the event by construction. One override is declared: the derivation would read coastal_municipal_governments as near-pure beneficiaries (they are declared beneficiaries and set the agenda), but their true structural relationship is mixed — they capture tax base while carrying deferred disaster liability and administering the theater, which places them moderately on the target side of symmetric; d is overridden to 0.35 for the institutional power atom. Scope is regional: the arrangement's boundary was legible in principle (the stones state it), but its costs verify only at recurrence — which is what lets regional scope amplify effective extraction on the seat that cannot verify anything until it is too late.
 *
 * MANDATROPHY ANALYSIS:
 *   The arrangement still trades on its founding authority — ancestral instruction, the moral weight of the dead — while performing none of the protection that authority was earned by; the mandate (honor and heed the ancestors' line) has outlived the function (keep dwellings above it). That is the mandatrophy shape, and it is why the R5 interview here pairs a live founding problem with an arrangement that abandoned it: the hazard is live (2011 corroborated it at greater magnitude), the arrangement persists, and the persistence runs on borrowed authority rather than on service. The classification discipline matters in both directions: reading the husk as coordination (memory protects) would launder the risk transfer as culture; reading it as pure coercion would invent an enforcer the record does not show — no seat actively maintains the non-bindingness, and the development class captures gains without administering anything. The degraded-remnant claim fits the record: atrophied function, high performance ratio, inertial persistence, an administrator that could redraw the building line but bears too little of the deferred cost to do it. The R5 mismatch consumer reads founding_problem_status (live) x disappearance_verdict (world_rearranges): no dead-problem zombie flag fires, because the problem is not dead — the arrangement simply stopped serving it, which is a different and more damning failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates one reading (commemorative_husk_reading) of the kernel tsunami_stone_commitment; would instantiating the sibling behavioral_competence_reading instead change the arrangement''s epsilon, beneficiary structure, and type wholesale?',
    'Author and run the sibling reading as its own constraint file over the same 1933-2011 interval, then compare per-seat classifications across the two files; the catastrophe_validation_axis file supplies the shared evidentiary test both readings are run against.',
    'Under the sibling reading the arrangement is a live norm-enforcement mechanism with low epsilon and no victim class; under this reading it is a husk transferring risk to future residents at high epsilon. The kernel''s classification is indexical to the reading, and only the sibling files'' existence makes the comparison computable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this constraint is one reading of the tsunami stone kernel; sibling readings instantiate structurally different constraints.').

omega_variable(
    compliance_causality_ambiguity,
    'Was the pre-2011 compliance pattern (most famously the survival of villages that had relocated above the carved lines) caused by norm enforcement flowing from the inscriptions, or by confounders — geographic isolation, economic marginality, post-disaster relocation subsidies, seawall siting?',
    'Comparative village-level analysis of stone proximity against settlement-elevation decisions across the Sanriku coast, controlling for geography, land scarcity, and subsidy flows; oral-history corroboration of whether siting decisions ever cited the inscriptions as operative reasons.',
    'If inscription-caused, the sibling behavioral_competence reading wins and this file''s epsilon collapses toward coordination cost; if coincidental or weakly enforced, this reading stands with high epsilon on future residents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_causality_ambiguity, empirical, 'The core causal dispute: whether the stones ever bound behavior or only commemorated it.').

omega_variable(
    persistence_mechanism_inertia_vs_maintenance,
    'Does the husk arrangement persist by pure inertia and ceremony, or by active maintenance — seawall politics, development pressure on zoning, heritage budgets that absorb the stones into tourism?',
    'Process-trace municipal zoning decisions and seawall funding from 1960 to 2011: if non-bindingness required recurring active defense against relocation proposals, the arrangement is actively enforced; if it survived on default and ceremony, it persists inertially.',
    'Active maintenance would push the arrangement toward an enforced hybrid with a maintaining beneficiary seat; inertial persistence supports the degraded-remnant classification claimed here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_mechanism_inertia_vs_maintenance, empirical, 'What actually holds the husk arrangement in place across the interval.').

omega_variable(
    false_security_attribution,
    'Did the commemorative-plus-seawall framing actively raise exposure (protective infrastructure inducing denser, lower, closer rebuilding) or merely fail to lower it?',
    'Compare settlement density and floor-elevation decisions before and after seawall completion, and against comparable villages without seawalls, controlling for land scarcity and reconstruction subsidies.',
    'Active false security raises this reading''s epsilon above the authored 0.72 and makes the commemorative framing a causal extraction mechanism rather than a passive omission.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_security_attribution, empirical, 'Whether the remembrance framing contributed to the risk it now memorializes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__commemorative_husk_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsunami_stone_husk_tr_t1933, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 1933, 0.28).
narrative_ontology:measurement(tsunami_stone_husk_tr_t1950, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 1950, 0.4).
narrative_ontology:measurement(tsunami_stone_husk_tr_t1965, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 1965, 0.52).
narrative_ontology:measurement(tsunami_stone_husk_tr_t1980, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 1980, 0.62).
narrative_ontology:measurement(tsunami_stone_husk_tr_t1995, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 1995, 0.7).
narrative_ontology:measurement(tsunami_stone_husk_tr_t2011, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 2011, 0.75).

% Extraction over time
narrative_ontology:measurement(tsunami_stone_husk_be_t1933, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 1933, 0.45).
narrative_ontology:measurement(tsunami_stone_husk_be_t1950, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 1950, 0.52).
narrative_ontology:measurement(tsunami_stone_husk_be_t1965, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 1965, 0.6).
narrative_ontology:measurement(tsunami_stone_husk_be_t1980, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 1980, 0.66).
narrative_ontology:measurement(tsunami_stone_husk_be_t1995, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 1995, 0.7).
narrative_ontology:measurement(tsunami_stone_husk_be_t2011, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 2011, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(tsunami_stone_husk_su_t1933, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 1933, 0.55).
narrative_ontology:measurement(tsunami_stone_husk_su_t1950, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 1950, 0.48).
narrative_ontology:measurement(tsunami_stone_husk_su_t1965, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 1965, 0.42).
narrative_ontology:measurement(tsunami_stone_husk_su_t1980, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 1980, 0.37).
narrative_ontology:measurement(tsunami_stone_husk_su_t1995, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 1995, 0.33).
narrative_ontology:measurement(tsunami_stone_husk_su_t2011, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 2011, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__commemorative_husk_reading, identity_coordination).
narrative_ontology:affects_constraint(tsunami_stone_commitment__commemorative_husk_reading, behavioral_competence_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__commemorative_husk_reading, catastrophe_validation_axis).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (tsunami_stone_commitment), three readings, three files. The natural-language label 'the tsunami stones' conflates structurally distinct claims: whether the inscription retained causal force over settlement behavior (behavioral_competence_reading), what the 2011 event proved (catastrophe_validation_axis), and what the standing arrangement does now that the inscription's force has decayed (this file). Each file carries its own epsilon, beneficiaries, victims, and type; this file's epsilon (0.72) is authored for the husk arrangement under this reading's lights and is not comparable to the siblings' epsilon without their structural data. Edges here link the family for contamination and comparison analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tsunami_stone_commitment__commemorative_husk_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
