% ============================================================================
% CONSTRAINT STORY: second_amendment_text__originalist_civic_virtue_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__originalist_civic_virtue_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_text__originalist_civic_virtue_reading
 *   human_readable: Second Amendment as Guarantor of Citizen-Soldier Civic Republican Capacity
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This story instantiates the originalist civic-virtue reading of the
 *   Second Amendment's contested kernel: the claim that the founding-era
 *   'militia' meant the universal armed citizenry itself, and that the
 *   constitutional right protects the capacity of citizens to serve as
 *   citizen-soldiers rather than either an individual self-defense
 *   entitlement or a state-regulable collective security apparatus. This
 *   reading is distinct from, and does not attempt to adjudicate between, the
 *   individual_right_reading (personal self-defense as core protected
 *   activity, militia service unnecessary) or the collective_security_reading
 *   (right conditioned on organized state-sanctioned defense structures).
 *   Each reading is authored as its own constraint with its own epsilon; this
 *   file addresses only the civic-virtue reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__originalist_civic_virtue_reading, 0.18).
domain_priors:suppression_score(second_amendment_text__originalist_civic_virtue_reading, 0.28).
domain_priors:theater_ratio(second_amendment_text__originalist_civic_virtue_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__originalist_civic_virtue_reading, rope).
narrative_ontology:human_readable(second_amendment_text__originalist_civic_virtue_reading, "Second Amendment as Guarantor of Citizen-Soldier Civic Republican Capacity").
narrative_ontology:topic_domain(second_amendment_text__originalist_civic_virtue_reading, "constitutional_law/political_theory/firearms_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__originalist_civic_virtue_reading, 'f9cd30e6-69d9-4454-b5f5-d3304a00451a').
narrative_ontology:cs_kernel_codification('f9cd30e6-69d9-4454-b5f5-d3304a00451a', fixed_text).
narrative_ontology:cs_authority_grounding('f9cd30e6-69d9-4454-b5f5-d3304a00451a', lineage).
narrative_ontology:cs_interpretation_layer_present('f9cd30e6-69d9-4454-b5f5-d3304a00451a').
narrative_ontology:cs_reading_relation('f9cd30e6-69d9-4454-b5f5-d3304a00451a', second_amendment_text__collective_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9cd30e6-69d9-4454-b5f5-d3304a00451a', second_amendment_text__individual_right_reading, influences).
narrative_ontology:cs_axiom('f9cd30e6-69d9-4454-b5f5-d3304a00451a', foundational, right_exercised_through_civic_militia_capacity).
narrative_ontology:cs_axiom_status(right_exercised_through_civic_militia_capacity, holdable).
narrative_ontology:cs_axiom_grounding('f9cd30e6-69d9-4454-b5f5-d3304a00451a', right_exercised_through_civic_militia_capacity, conventional).
narrative_ontology:cs_axiom('f9cd30e6-69d9-4454-b5f5-d3304a00451a', secondary, standing_armies_are_constitutionally_disfavored).
narrative_ontology:cs_axiom_status(standing_armies_are_constitutionally_disfavored, holdable).
narrative_ontology:cs_axiom_grounding('f9cd30e6-69d9-4454-b5f5-d3304a00451a', standing_armies_are_constitutionally_disfavored, empirically_contingent).
narrative_ontology:cs_reference_frame('f9cd30e6-69d9-4454-b5f5-d3304a00451a', founding_era_universal_militia_civic_republicanism).
narrative_ontology:cs_drift_state('f9cd30e6-69d9-4454-b5f5-d3304a00451a', contemporary_post_heller_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f9cd30e6-69d9-4454-b5f5-d3304a00451a', '').
narrative_ontology:cs_kernel_id(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, the_political_community).
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, civic_republican_polity).
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, eligible_militia_age_citizens).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, citizen_soldier_ideal).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, distrust_of_standing_armies).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, civic_virtue_as_constitutional_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The political community as a collective entity is imagined by this reading as the thing the right ultimately protects: a body of citizens capable of common defense and resistant to both foreign conquest and domestic usurpation. It does not act; it is invoked as the abstract beneficiary of the arrangement.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, civic_republican_polity, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(second_amendment_text__originalist_civic_virtue_reading, civic_republican_polity).

% Founding-era free adult male citizens (and, in the modern extrapolation some originalist civic-virtue scholars draw, the broader body of civically eligible citizens) are understood to hold the capacity and expectation of bearing arms in defense of the community. They keep weapons not primarily for personal defense or hunting but as a standing readiness to be called into common service; their relationship to the right is participatory rather than proprietary.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, eligible_militia_age_citizens, beneficiary,
    moderate, generational, constrained, national).

% Historians and constitutional theorists who reconstruct founding-era militia practice and civic-republican political theory to argue this reading against both the individual-right and collective-security camps. They administer the interpretive framework through scholarship, amicus briefs, and judicial argument, but hold no coercive enforcement power themselves; their leverage is persuasive and doctrinal.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, originalist_legal_scholars, agenda_setter,
    organized, generational, mobile, national).

% The founding generation's ideological distrust of professional standing armies as instruments of tyranny is the animating premise this reading vindicates. It is a historical political commitment, not an actor, but this reading treats its vindication as central to what the clause accomplishes.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, standing_army_skeptics_historical, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(second_amendment_text__originalist_civic_virtue_reading, standing_army_skeptics_historical).

% Citizens living in contexts where organized militia participation has no contemporary institutional analog (no muster, no communal armory, no civic expectation of armed service) do not fit neatly into the citizen-soldier frame this reading presupposes. They are largely absent from the civic-virtue argument's own terms, since the reading's beneficiary is the participatory citizen-soldier, not the atomized modern gun owner or the person seeking protection from gun violence.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, modern_urban_dwellers_disconnected_from_militia_practice, excluded,
    powerless, biographical, trapped, national).

% Communities bearing the practical costs of widespread firearm access have no seat within this reading's own framework, which is organized around civic-republican defense capacity rather than harm distribution. Whether or how this reading bears on their situation is left to downstream policy debates the reading itself does not resolve.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, gun_violence_affected_communities, excluded,
    powerless, immediate, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves, as founding constitutional commitment, the capacity of the citizenry as a body to constitute an armed common-defense force independent of a standing professional army, understood as both a practical defense mechanism and a check against governmental tyranny.
% TRANSFER_FUNCTION: Moves interpretive authority and legitimacy toward readings of gun regulation that ask whether a given citizen or class of citizens is being kept within reach of militia-style civic capacity, and away from readings organized purely around individual self-defense utility or organized-state regulatory prerogative.
% ABSENT_VOICES: Contemporary gun-violence-affected communities and urban residents disconnected from any living militia tradition are not addressed within the reading's own terms; individual-right proponents object that civic-virtue framing subordinates personal self-defense to a communitarian purpose many gun owners do not share; collective-security proponents object that the reading strips the state's regulatory role from a clause whose grammar foregrounds a 'well regulated militia.'
% DISAPPEARANCE_RATIONALE: If this specific reading vanished from constitutional discourse, the operative constitutional text would remain, and litigation would proceed under whichever sibling reading (individual-right or collective-security) courts adopted instead — the world does not rearrange around this reading uniquely, since it is one interpretive lens among three live contenders rather than an independently operative arrangement. Originalist scholars would say serious historical work on founding intent would be lost from the discourse; other camps would say little would change in practical doctrine, since this reading has never been the controlling one in modern jurisprudence.
% FOUNDING_PROBLEM: The founding generation sought to avoid dependence on a standing professional army (associated with British occupation and monarchical tyranny) by ensuring the citizenry itself retained the capacity for organized armed defense, rooted in Anglo-American militia tradition and civic-republican political theory.
% FOUNDING_PROBLEM_CORROBORATION: Military historians and scholars of civic-republican political thought outside the gun-rights advocacy ecosystem (e.g. specialists in Anglo-American militia history and 18th-century political theory) corroborate that the historical militia system and the ideological distrust of standing armies were real founding-era phenomena; however, these same historians frequently dispute whether the citizen-soldier ideal maps onto, or was intended to authorize, an individual constitutional entitlement detached from organized militia service — the corroboration supports the historical premise while leaving the doctrinal conclusion contested.
narrative_ontology:disappearance_verdict(second_amendment_text__originalist_civic_virtue_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_text__originalist_civic_virtue_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__originalist_civic_virtue_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_text__originalist_civic_virtue_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__originalist_civic_virtue_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__originalist_civic_virtue_reading_tests).
:- end_tests(second_amendment_text__originalist_civic_virtue_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low because this reading, taken on its own terms, describes a coordination good (collective defense capacity, a check on standing-army tyranny) with no identified victim class extracting rents from others through the mechanism. Theater ratio rises substantially over the measured interval (0.10 to 0.42) because the practical institution the reading valorizes — an actual functioning militia of armed citizens subject to muster and communal military obligation — has almost entirely disappeared from American civic life since the 19th century, replaced by the National Guard and professional military, while the constitutional-theoretical invocation of 'citizen-soldier capacity' persists in legal and political rhetoric detached from any living institutional referent. Accessibility collapse is moderate (0.4): the historical militia practice this reading describes is not fully forgotten (well-documented in law and history) but is functionally inaccessible as a lived civic institution. Resistance is substantial (0.6) because this reading is actively and vigorously contested by both sibling readings in courts, legislatures, and public discourse — it has never achieved uncontested doctrinal dominance.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting originalist scholarly seat, this reading is a historically grounded coordination account: it recovers what the text actually meant to its framers and describes a genuine public good. From the excluded seats (gun-violence-affected communities, urban residents with no militia-adjacent civic life), the reading is simply not responsive to their situation — not extractive, but structurally silent. The engine should be expected to classify this reading close to rope or, given the theater trajectory, drifting toward piton (a once-functional coordination structure whose institutional referent has substantially atrophied while symbolic/doctrinal invocation persists).
 *
 * DIRECTIONALITY LOGIC:
 *   The declared beneficiary is the political community and the class of civically-eligible citizens, understood collectively rather than as individuated rights-holders; this produces a low, diffuse directionality profile rather than a concentrated one, because no single actor captures a rent — the good described is a public good of collective defense capacity and tyranny-deterrence. No victim group is authored: this reading, unlike a regulatory-capture or extraction story, does not identify a class structurally paying a cost through the mechanism. The excluded stakeholders (modern urban dwellers, gun-violence-affected communities) are not victims of THIS constraint's operation — they are voices the reading's own conceptual apparatus does not address, which is a five-questions absence, not an extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as contested rather than flatly dead, because the underlying ideological commitment (distrust of standing armies, valorization of citizen-soldier capacity) is a live political-theoretical position for originalist scholars even though the literal institutional referent (functioning state militias of universally-armed citizens) has been institutionally superseded by the National Guard and professional armed forces. This is precisely the kind of divergence the R5 genealogy interview is built to surface: a reading whose founding problem's original institutional form is largely gone, but whose proponents maintain the underlying value commitment remains live and constitutionally operative independent of the vanished institution. The theater_ratio trajectory (rising from 0.10 to 0.42) documents this gap empirically — an increasing share of invocation is symbolic-constitutional rather than tied to functioning militia practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civic_virtue_vs_individual_right_kernel_disagreement,
    'Does ''the right of the people to keep and bear arms'' in the operative clause refer to a right exercised through militia participation (this reading) or an individual right severable from any militia nexus (individual_right_reading)? This is the central textual and historical fork the kernel contest turns on.',
    'No further historical evidence is likely to definitively settle this; it is a live, evidence-informed but ultimately interpretive disagreement resolved (provisionally, and only within a given jurisdiction and era) by controlling judicial precedent, not by discovery of new founding-era documents alone, since existing evidence has already been extensively mined by both camps.',
    'If courts adopt this reading over individual_right_reading, regulation nexus-tied to organized civic defense capacity becomes constitutionally salient in ways that pure individual-right doctrine would not permit; if individual_right_reading prevails (as it substantially has post-Heller), this reading''s beneficiary-of-the-collective framing becomes doctrinally marginal regardless of its historical merit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civic_virtue_vs_individual_right_kernel_disagreement, conceptual, 'The core fork between this reading and individual_right_reading over whether the right is collectively- or individually-anchored.').

omega_variable(
    militia_referent_extinction,
    'Does the near-total disappearance of the founding-era institutional militia (universal armed citizen muster) as a living practice mean this reading''s underlying coordination function has become vestigial (piton-like), even if the value commitment it vindicates (anti-standing-army, civic virtue) remains philosophically live?',
    'Track whether any modern institution (organized state militias, unorganized militia statutes, National Guard) functions as a genuine successor performing the coordination role this reading attributes to the founding militia; absence of such a successor over a sustained period is evidence for vestigial/theatrical persistence.',
    'If the coordination function is vestigial, the reading functions today primarily as constitutional rhetoric supporting other policy conclusions rather than as a description of an operating civic institution — pushing classification toward piton rather than rope despite the low authored extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_referent_extinction, empirical, 'Whether the citizen-soldier institutional referent has atrophied into symbolic invocation.').

omega_variable(
    collective_beneficiary_naturality,
    'Is ''the political community'' or ''civic republican polity'' a genuine natural beneficiary class, or is invoking a diffuse collective beneficiary itself a rhetorical move that obscures which concrete actors (gun manufacturers, advocacy organizations, particular political coalitions) actually benefit from this reading''s doctrinal adoption?',
    'Examine funding, advocacy, and beneficiary patterns of organizations that promote the civic-virtue reading specifically (as distinct from the individual-right reading) to determine whether concrete institutional actors capture disproportionate benefit from its adoption.',
    'If concrete organized interests are the true beneficiaries behind the diffuse collective framing, the low authored extractiveness would be understated and the reading would sit closer to a tangled_rope or even a false-summit dynamic; if the beneficiary is genuinely diffuse, the low extraction is well-supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_beneficiary_naturality, conceptual, 'Whether the diffuse-collective beneficiary framing conceals concrete capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__originalist_civic_virtue_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement(seco_tr_t1860, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1860, 0.18).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1900, 0.28).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1939, 0.35).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1980, 0.4).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 2008, 0.41).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1791, 0.08).
narrative_ontology:measurement(seco_be_t1860, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1860, 0.1).
narrative_ontology:measurement(seco_be_t1900, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1900, 0.12).
narrative_ontology:measurement(seco_be_t1939, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1939, 0.14).
narrative_ontology:measurement(seco_be_t1980, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1980, 0.16).
narrative_ontology:measurement(seco_be_t2008, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 2008, 0.17).
narrative_ontology:measurement(seco_be_t2024, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 2024, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(second_amendment_text__originalist_civic_virtue_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__originalist_civic_virtue_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__individual_right_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'Second Amendment' kernel per the eps-invariance principle. collective_security_reading ties the right to organized, state-regulable militia service; individual_right_reading treats personal self-defense as the core protected activity independent of militia service; this file (originalist_civic_virtue_reading) locates the beneficiary in the political community's collective civic-republican defense capacity, tied to founding-era universal-militia history. Each carries a distinct epsilon, distinct beneficiary/victim structure, and distinct claimed_type; they are linked via network.affects_constraints rather than merged into one measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
