% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__commemorative_husk_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: aneyoshi_land_use_prohibition__commemorative_husk_reading
 *   human_readable: Aneyoshi Tsunami Stone Line — Commemorative Husk Reading
 *   domain: disaster_anthropology/commitment_systems
 *
 * SUMMARY:
 *   On the Sanriku coast, tsunami stones inscribed after the 1896 and 1933
 *   disasters instructed descendants never to build below a marked line. This
 *   story instantiates the reading under which that instruction decayed into
 *   commemoration: the generation with direct experiential memory of the
 *   water reached its physical limit within decades, informal social
 *   enforcement (elders refusing to sell or bless lowland construction,
 *   communal shaming of builders below the line) attenuated as memory
 *   receded, and by the late twentieth century the stone functioned primarily
 *   as a heritage marker rather than an operative zoning constraint.
 *   Development proceeded below several such lines well before 2011. This is
 *   a deliberate companion to the sibling story
 *   (behavioral_competence_reading, not authored here), which holds that the
 *   Aneyoshi stone specifically remained operationally binding for 78 years
 *   and is credited with the village's survival in 2011. Both readings share
 *   the same physical artifact and inscription; they diverge entirely on
 *   whether the prohibition retained behavioral force, which drives opposite
 *   classifications and opposite victim/beneficiary structures. This story's
 *   ε is high because, on this reading, the stone's persistence as mere
 *   symbol is precisely what let development capture the land it was meant to
 *   keep empty — the commemorative function survived, the constraining
 *   function died, and that asymmetry is the extraction.
 *
 * KEY AGENTS:
 *   - coastal_developers: primary beneficiary (organized/mobile) — captures lowland value the prohibition would have foreclosed
 *   - future_below_line_residents: primary victim (powerless/trapped) — inherits exposure without inheriting the warning's operative force
 *   - municipal_land_tax_base: institutional beneficiary — gains taxable parcels from non-enforcement
 *   - elder_survivor_descendants: excluded voice — holds the memory of the stone's original operative meaning but lacks planning standing
 *   - disaster_researchers: analytical observer — documents the decay pattern across the coast
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.71).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.18).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__commemorative_husk_reading, "Aneyoshi Tsunami Stone Line — Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__commemorative_husk_reading, "disaster_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__commemorative_husk_reading, '6f386310-fce7-44b0-bfe0-96e9e70941f7').
narrative_ontology:cs_kernel_codification('6f386310-fce7-44b0-bfe0-96e9e70941f7', fixed_text).
narrative_ontology:cs_authority_grounding('6f386310-fce7-44b0-bfe0-96e9e70941f7', practice).
narrative_ontology:cs_reading_relation('6f386310-fce7-44b0-bfe0-96e9e70941f7', aneyoshi_land_use_prohibition__behavioral_competence_reading, forecloses).
narrative_ontology:cs_axiom('6f386310-fce7-44b0-bfe0-96e9e70941f7', foundational, inscribed_warning_loses_force_absent_living_memory).
narrative_ontology:cs_axiom_status(inscribed_warning_loses_force_absent_living_memory, holdable).
narrative_ontology:cs_axiom_grounding('6f386310-fce7-44b0-bfe0-96e9e70941f7', inscribed_warning_loses_force_absent_living_memory, empirically_contingent).
narrative_ontology:cs_axiom('6f386310-fce7-44b0-bfe0-96e9e70941f7', secondary, commemorative_status_is_evidence_of_lapsed_not_active_prohibition).
narrative_ontology:cs_axiom_status(commemorative_status_is_evidence_of_lapsed_not_active_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('6f386310-fce7-44b0-bfe0-96e9e70941f7', commemorative_status_is_evidence_of_lapsed_not_active_prohibition, empirically_contingent).
narrative_ontology:cs_reference_frame('6f386310-fce7-44b0-bfe0-96e9e70941f7', stone_as_operative_boundary_marker_1933).
narrative_ontology:cs_drift_state('6f386310-fce7-44b0-bfe0-96e9e70941f7', pre_2011_tsunami, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('6f386310-fce7-44b0-bfe0-96e9e70941f7', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, coastal_developers).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, municipal_land_tax_base).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, tourism_and_heritage_office).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, future_below_line_residents).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, renters_in_new_lowland_construction).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__commemorative_husk_reading, memorial_stones_preserve_historical_memory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build housing, guesthouses, and commercial structures below the inscribed stone line where land is cheaper and closer to the harbor and road network. Treat the stone as a heritage marker rather than a binding line; face no zoning enforcement that would block construction below it. Capture the value of otherwise-forgone waterfront land.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, coastal_developers, beneficiary,
    organized, biographical, mobile, regional).

% Represents the fiscal interest of the town in maximizing developable, taxable land area. Every parcel that remains undeveloped above a strictly-enforced line is a parcel producing no property tax revenue; the town's finances benefit structurally from a reading of the stone as symbolic rather than as an active land-use restriction.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, municipal_land_tax_base, beneficiary,
    institutional, generational, analytical, regional).
narrative_ontology:stakeholder_non_agent(aneyoshi_land_use_prohibition__commemorative_husk_reading, municipal_land_tax_base).

% Curates the stone as a memorial destination and disaster-education site — a stop for visiting officials, schoolchildren, and journalists after 2011. Benefits from the stone's meaning as commemorative artifact; has no operational stake in, and does not enforce, any behavioral prohibition attached to it.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, tourism_and_heritage_office, beneficiary,
    moderate, biographical, mobile, national).

% Households who move into or are born into housing built below the stone's inscribed line, without personal memory of the 1896 or 1933 tsunamis the stone commemorates. Inherit the geographic exposure the stone was erected to prevent, without inheriting any operative rule that would have kept the land unbuilt. Have no way to know, from the built environment alone, that they occupy ground the stone's original framers intended to remain empty.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, future_below_line_residents, payer,
    powerless, generational, trapped, local).

% Rent housing built below the line because it is cheaper and more convenient than upslope alternatives. Bear the tsunami-return risk directly and immediately; have essentially no leverage over zoning decisions made by the municipality or developers, and no ability to verify at the point of renting whether the location sits below a historically load-bearing warning line.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, renters_in_new_lowland_construction, payer,
    powerless, immediate, constrained, local).

% Descendants of the 1933 survivors who erected or maintained the stone as an operative warning, not a museum piece. Would object that the stone's meaning has been hollowed into commemoration precisely so that development could proceed below it, but are rarely consulted in municipal land-use planning and have no formal standing to block construction on the basis of the inscription.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, elder_survivor_descendants, excluded,
    powerless, generational, identity_locked, local).

% Study the stone as a case in disaster memory and institutional decay — comparing pre-2011 land use around the marker to development patterns elsewhere on the Sanriku coast. Document how quickly an inscribed prohibition can shift from binding rule to interpretive artifact once the generation that enforced it through direct memory and informal social sanction has passed.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, disaster_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_land_use_prohibition__commemorative_husk_reading, coastal_developers).
narrative_ontology:fixing_cost_class(aneyoshi_land_use_prohibition__commemorative_husk_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally, the stone coordinated settlement decisions across generations who had no other durable channel for transmitting tsunami-line knowledge; on this reading, that coordination function has lapsed and what remains is a shared commemorative object with no operative planning role.
% TRANSFER_FUNCTION: Moves the option value of buildable lowland from a diffuse future population (whoever will occupy the exposed land when the next tsunami arrives) to present developers, present renters seeking cheap housing, the present tax base, and the tourism sector that monetizes the stone's symbolic meaning without needing it to constrain anything.
% ABSENT_VOICES: Future residents who will occupy the land during the next tsunami event are definitionally absent from any current planning conversation; elder survivor descendants who remember the stone's operative use are present in the community but structurally sidelined in zoning process.
% DISAPPEARANCE_RATIONALE: On this reading, if the stone were removed or its inscription forgotten tomorrow, current land-use decisions would not measurably change: development below the line is already proceeding without the stone functioning as an enforced constraint. The stone's removal would be experienced as a loss of heritage and memorial value, not as the removal of an active planning rule — which is precisely the reading's claim: the behavioral force is already gone, so its physical disappearance would rearrange commemorative practice, not construction patterns.
% FOUNDING_PROBLEM: The 1933 Showa Sanriku tsunami (and the 1896 Meiji tsunami before it) killed large fractions of coastal village populations who had rebuilt on low ground after the previous disaster; the stone was erected to physically mark, for people without written planning records, exactly how far the water reached and where it was therefore unsafe to live.
% FOUNDING_PROBLEM_CORROBORATION: Independent disaster-anthropology researchers and post-2011 journalistic surveys of Sanriku coast land use corroborate, from outside both the development interests and the tourism office, that residential construction has occurred below tsunami-stone lines at multiple sites in the decades before 2011 with no municipal enforcement action tied to the inscriptions — supporting the claim that the prohibition's behavioral force lapsed well before the stone's post-2011 commemorative renaissance.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__commemorative_husk_reading, world_unchanged).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises steadily from 0.08 (1933, stone freshly erected, memory vivid, prohibition effectively self-enforcing) to 0.71 (2011, prohibition fully decayed into symbol, development normalized below the line). Theater ratio tracks even more sharply upward (0.05 to 0.82) because the stone's commemorative/performative role (marker, occasional ceremony, later tourist site) persisted and even intensified while its behavioral-restriction role evaporated — the classic piton signature of form outlasting function. Suppression is authored low (0.18) because on this reading there is no active enforcement machinery at all being exercised against would-be builders; the absence of suppression is itself part of the claim — nothing coercive stands between developers and the land, which is why extraction can rise without resistance escalating in kind. Accessibility collapse is moderate (0.35): building below the line was never physically or legally blocked on this reading, so alternatives to development never meaningfully collapsed — they were simply not pursued out of habit for as long as memory lasted, then abandoned.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat (functionally, no one currently administers the prohibition — the stone has no zoning authority), the constraint reads as pure heritage object, entirely benign. From the payer seats (future and current lowland residents), the same stone represents a warning whose institutional carrier decayed while the risk it named did not. The engine should compute divergent seat classifications: developer and tax-base seats see something closer to a non-binding relic; the powerless trapped and constrained payer seats sit on the sharp end of a structure that looks, from where they stand, like a promise that was made and then quietly not kept.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers and the municipal tax base are declared beneficiaries because non-enforcement of the line converts previously-foreclosed land into revenue and profit — their directionality sits near the full-beneficiary end. Future below-line residents and renters are declared victims because they bear the tsunami-return risk the stone was built to prevent, with no operative mechanism protecting them and no ability to exit the exposure cheaply (trapped/constrained) — their directionality sits near the full-target end. The tourism office is a milder beneficiary: it profits from the stone's meaning without needing the underlying prohibition to bind anything, which is exactly the commemorative-husk pattern this reading names.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading treats the founding problem (transmitting empirical tsunami-line knowledge across generations without written planning infrastructure) as dead in practice while the stone persists as an active commemorative object — a textbook mandatrophy signature: the mandate (behavioral prohibition) has lapsed but the institution (the inscribed stone, its ceremonies, its heritage status) not only continues but was arguably strengthened by the very same passage of time that killed its operative force. Classifying this as piton rather than snare prevents mislabeling the situation as active predation — no one is coercively extracting anything at the moment of decay; the extraction is a passive byproduct of a warning system's carrier surviving its content. It also prevents mislabeling it as a functioning rope: there is no live coordination happening, only its afterimage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aneyoshi_specific_vs_regional_pattern,
    'Did the Aneyoshi stone specifically retain behavioral force through 2011 (as local reporting after the tsunami claimed, citing the village''s survival with the marker), even if OTHER Sanriku tsunami stones decayed into pure commemoration over the same period?',
    'Site-specific historical land registry review for Aneyoshi comparing settlement patterns above/below the inscribed line from 1933-2011, cross-referenced against comparable review at nearby villages whose stones are documented as having been built below.',
    'If Aneyoshi is empirically exceptional (its specific stone retained force) while the regional pattern generally decayed, this story''s high-extractiveness reading applies accurately to the regional pattern but may misclassify the Aneyoshi site itself, which would belong under the sibling reading instead. The two constraints would then apply to different physical markers rather than being two readings of literally the same stone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aneyoshi_specific_vs_regional_pattern, empirical, 'Whether the commemorative-husk pattern generalizes to the specific Aneyoshi marker or only to the regional class of tsunami stones.').

omega_variable(
    mechanism_of_decay,
    'Was the loss of behavioral force (on this reading) driven by generational memory attrition, active development pressure overriding known risk, or municipal non-enforcement despite community objection?',
    'Oral history interviews with elder survivor descendants and municipal planning records review to distinguish passive forgetting from active override.',
    'Passive memory attrition supports a piton classification (inertial decay, no identifiable extractor); active override in the face of known objection would push the classification toward snare (identifiable beneficiaries suppressing a known warning for profit).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mechanism_of_decay, conceptual, 'Whether the prohibition''s decay was passive institutional atrophy or active suppression of a known risk.').

omega_variable(
    kernel_reading_selection_criterion,
    'What evidentiary standard should determine which reading of the Aneyoshi kernel is correct — is 78-year continuous behavioral enforcement a claim that can be verified against land-registry and settlement records, or is it partly a retrospective narrative constructed after the stone''s 2011 vindication made a strong story newsworthy?',
    'Comparison of pre-2011 (before the stone became internationally famous) versus post-2011 secondary sourcing on Aneyoshi land-use history; pre-2011 sources are less subject to narrative-construction pressure toward the behavioral-competence reading.',
    'If pre-2011 sourcing is sparse, the behavioral_competence_reading may be partly a retrospective construction and this commemorative_husk_reading''s decay pattern may be closer to the honest baseline; if pre-2011 land records independently show the line was respected, the sibling reading has stronger independent grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_criterion, conceptual, 'Whether reading selection is decidable by pre-existing evidence or is partly shaped by post-disaster narrative construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__commemorative_husk_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 1933, 0.05).
narrative_ontology:measurement_basis(aney_tr_t1933, observed).
narrative_ontology:measurement(aney_tr_t1950, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement_basis(aney_tr_t1950, observed).
narrative_ontology:measurement(aney_tr_t1965, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 1965, 0.28).
narrative_ontology:measurement_basis(aney_tr_t1965, observed).
narrative_ontology:measurement(aney_tr_t1980, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 1980, 0.45).
narrative_ontology:measurement_basis(aney_tr_t1980, observed).
narrative_ontology:measurement(aney_tr_t1995, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 1995, 0.63).
narrative_ontology:measurement_basis(aney_tr_t1995, observed).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 2011, 0.82).
narrative_ontology:measurement_basis(aney_tr_t2011, observed).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 1933, 0.08).
narrative_ontology:measurement_basis(aney_be_t1933, observed).
narrative_ontology:measurement(aney_be_t1950, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement_basis(aney_be_t1950, observed).
narrative_ontology:measurement(aney_be_t1965, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 1965, 0.32).
narrative_ontology:measurement_basis(aney_be_t1965, observed).
narrative_ontology:measurement(aney_be_t1980, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement_basis(aney_be_t1980, observed).
narrative_ontology:measurement(aney_be_t1995, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement_basis(aney_be_t1995, observed).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 2011, 0.71).
narrative_ontology:measurement_basis(aney_be_t2011, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_land_use_prohibition__commemorative_husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__commemorative_husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.08).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% This story and aneyoshi_land_use_prohibition__behavioral_competence_reading are sibling readings of a single kernel (the Aneyoshi tsunami stone's inscribed prohibition). They share the identical physical artifact and text but diverge on the empirical question of continued behavioral force across 1933-2011. This reading (commemorative_husk) authors high extractiveness and a piton classification on the premise that the prohibition decayed into symbol; the sibling authors low extractiveness and a rope/mountain-adjacent classification on the premise that the prohibition was operationally enforced continuously. The two ε values are intentionally divergent per the ε-invariance principle: this is not the same constraint measured two ways, but two structurally distinct claims about what actually happened at the site, each with its own coherent metric profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
