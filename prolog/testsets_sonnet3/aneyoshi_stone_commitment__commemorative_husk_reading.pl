% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__commemorative_husk_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: aneyoshi_stone_commitment__commemorative_husk_reading
 *   human_readable: Aneyoshi Tsunami Stone — Commemorative Husk Reading (Decayed Behavioral Directive)
 *   domain: disaster_anthropology/commitment_systems
 *
 * SUMMARY:
 *   In 1933, survivors of a devastating tsunami in Aneyoshi, Japan, carved a
 *   warning stone instructing descendants never to build homes below its
 *   marked elevation. This story instantiates the commemorative_husk_reading
 *   of the contested Aneyoshi stone kernel: under this reading, the
 *   inscription's behavioral force decayed across the 78 years between
 *   carving and the 2011 Tohoku tsunami, such that by 2011 the stone
 *   functioned as a heritage marker and moral parable rather than an
 *   operative land-use constraint. Settlement patterns in the intervening
 *   decades were driven by land economics, road access, and fishing-industry
 *   proximity, not by consultation of the inscription. The sibling reading
 *   (behavioral_competence_reading) holds that the stone retained operational
 *   force in real building-location decisions across the same interval — that
 *   is a different constraint, authored separately, with its own ε. This
 *   reading's ε is high (0.71) precisely because it asserts that the
 *   coordination function had already failed by the time of the 2011 event,
 *   making any credit assigned to the stone for survivors' good fortune a
 *   retrospective narrative overlay serving tourism and disaster-education
 *   institutions rather than a description of a live constraint.
 *
 * KEY AGENTS:
 *   - low_lying_settlement_residents: primary payer (powerless/trapped) — built below the marked line across generations without the stone functioning as an operative deterrent
 *   - future_coastal_households: secondary payer (powerless/constrained) — inherit a symbolic-only marker rather than a working land-use rule
 *   - local_tourism_and_heritage_bodies: beneficiary (organized/mobile) — extract visitor-economy and reputational value from the stone as artifact
 *   - post_disaster_narrative_curators: beneficiary/agenda_setter (institutional/analytical) — set the ancestral-wisdom interpretive frame that obscures the coordination failure
 *   - municipal_land_use_planners: agenda_setter (institutional/constrained) — the actual decision-makers, who do not treat the stone as legally operative
 *   - disaster_researchers: analytical observer — positioned to test the reading empirically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, 0.71).
domain_priors:suppression_score(aneyoshi_stone_commitment__commemorative_husk_reading, 0.18).
domain_priors:theater_ratio(aneyoshi_stone_commitment__commemorative_husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_commitment__commemorative_husk_reading, "Aneyoshi Tsunami Stone — Commemorative Husk Reading (Decayed Behavioral Directive)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__commemorative_husk_reading, "disaster_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__commemorative_husk_reading, '8c3e7181-6174-4ec6-8338-3adf86790067').
narrative_ontology:cs_kernel_codification('8c3e7181-6174-4ec6-8338-3adf86790067', fixed_text).
narrative_ontology:cs_authority_grounding('8c3e7181-6174-4ec6-8338-3adf86790067', lineage).
narrative_ontology:cs_interpretation_layer_present('8c3e7181-6174-4ec6-8338-3adf86790067').
narrative_ontology:cs_reading_relation('8c3e7181-6174-4ec6-8338-3adf86790067', aneyoshi_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('8c3e7181-6174-4ec6-8338-3adf86790067', foundational, commemorative_function_supersedes_directive_function).
narrative_ontology:cs_axiom_status(commemorative_function_supersedes_directive_function, holdable).
narrative_ontology:cs_axiom_grounding('8c3e7181-6174-4ec6-8338-3adf86790067', commemorative_function_supersedes_directive_function, empirically_contingent).
narrative_ontology:cs_axiom('8c3e7181-6174-4ec6-8338-3adf86790067', secondary, survival_attributable_to_elevation_not_compliance).
narrative_ontology:cs_axiom_status(survival_attributable_to_elevation_not_compliance, holdable).
narrative_ontology:cs_axiom_grounding('8c3e7181-6174-4ec6-8338-3adf86790067', survival_attributable_to_elevation_not_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('8c3e7181-6174-4ec6-8338-3adf86790067', id_1933_carved_warning_as_operative_settlement_boundary).
narrative_ontology:cs_drift_state('8c3e7181-6174-4ec6-8338-3adf86790067', id_2011_tohoku_tsunami, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('8c3e7181-6174-4ec6-8338-3adf86790067', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, local_tourism_and_heritage_bodies).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, post_disaster_narrative_curators).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, low_lying_settlement_residents).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, future_coastal_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Households that settled and built below the stone's marked line across the decades between 1933 and 2011, driven by land price, road access, fishing-economy proximity, and ordinary generational forgetting rather than any active decision to disregard the inscription. They bore the tsunami's impact in proportion to how far downhill they had settled; the stone did not function as a rule they consulted when choosing where to build.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, low_lying_settlement_residents, payer,
    powerless, generational, trapped, local).

% Households making settlement and rebuilding decisions after 2011 who inherit a landscape where the stone is treated as a memorial to consult emotionally, not a zoning instrument to consult practically; land-use permitting, seawalls, and elevation plans are set by prefectural and municipal engineering standards, not by the inscription's line.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, future_coastal_households, payer,
    powerless, generational, constrained, local).

% Organizations that maintain the stone as a heritage/tourism site and disaster-education waypoint, drawing visitors and media attention to a single surviving marker that photographs well and narrates a clean moral lesson. They collect reputational and visitor-economy value from the stone's symbolic status without bearing responsibility for whether present-day land use honors its directive.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, local_tourism_and_heritage_bodies, beneficiary,
    organized, biographical, mobile, regional).

% Government disaster-education programs, museums, and media that hold up the Aneyoshi stone as a national parable of ancestral wisdom vindicated by the 2011 tsunami. They set the interpretive frame in which the stone is presented, and benefit from a tidy causal story (ancestors warned, descendants who listened survived) that is easier to circulate than the messier truth that survival correlated with elevation and settlement pattern more than with anyone reading the inscription.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, post_disaster_narrative_curators, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__commemorative_husk_reading, post_disaster_narrative_curators, agenda_setter).

% The actual decision-makers on where new construction, seawalls, and elevation requirements go after 2011. They operate through engineering hazard maps and building codes; the stone's line is referenced in disaster-education materials but carries no legal force in permitting decisions, and planners do not treat it as an operative constraint.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, municipal_land_use_planners, agenda_setter,
    institutional, generational, constrained, regional).

% Anthropologists and disaster-risk scholars who study why some tsunami-stone communities survived and others did not, and who are positioned to test whether stone directives functioned as behavioral constraints or whether survival tracked confounds like elevation, population decline, and road infrastructure independent of the inscription.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__commemorative_husk_reading, post_disaster_narrative_curators).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__commemorative_husk_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None operative in the present tense. The stone was erected to coordinate future generations' settlement choices around a remembered hazard line; under this reading that coordination function had already atrophied into commemoration well before 2011, so there is no live coordination problem the stone currently solves for land-use decisions.
% TRANSFER_FUNCTION: Symbolic and narrative capital flows from the historical disaster event, through the stone-as-artifact, to heritage/tourism bodies and national disaster-education institutions who use it as proof-of-concept for ancestral wisdom. No transfer of behavioral compliance or cost avoidance flows to the residents whose settlement locations the stone was originally meant to constrain.
% ABSENT_VOICES: The 1933 carvers and their immediate descendants who intended the inscription as an operative rule are not present to contest the museumification of their warning; residents killed or displaced in 2011 who settled below the line cannot testify to whether they knew of, consulted, or disregarded the stone, leaving the behavioral-efficacy question empirically underdetermined from the payer side.
% DISAPPEARANCE_RATIONALE: Under this reading, if the stone vanished overnight, present-day land-use decisions would proceed exactly as they do now — governed by municipal hazard maps, engineering codes, and market pressure on buildable land — because the stone was not functioning as an input to those decisions. Tourism and disaster-education programming would need a substitute artifact, but the underlying settlement and permitting system would not rearrange.
% FOUNDING_PROBLEM: Coastal communities repeatedly rebuilt in tsunami-vulnerable low ground after each generation's memory of the previous disaster faded; the stone was carved specifically to transmit a hard settlement boundary ('do not build below this point') across generational amnesia.
% FOUNDING_PROBLEM_CORROBORATION: Independent disaster-risk researchers and post-2011 land-use audits (cited in academic and NHK-affiliated retrospectives on tsunami stones across Tohoku) attest that most communities with surviving stones did not use them as active planning inputs, and that survival in 2011 correlated more strongly with elevation, seawall placement, and depopulation-driven vacancy than with stone-directed compliance. This corroboration comes from researchers and planners outside the tourism/heritage bodies that benefit from the ancestral-wisdom narrative, though full corroboration is limited because pre-2011 resident-level land-use reasoning was never systematically recorded.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__commemorative_husk_reading, world_unchanged).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71) because under this reading the constraint's only remaining function is symbolic capture of a disaster-survival narrative that does not correspond to an operative behavioral mechanism — the 'coordination' being celebrated did not occur. Suppression is low (0.18): no one is coercively prevented from building below the line, which is itself part of the diagnosis — there is no active enforcement to suppress against, only quiet drift. Theater ratio rises sharply across the interval (0.10 at carving to 0.82 by 2011) because the stone's function shifts from (hypothetically) operative warning to almost pure commemorative performance as the founding generation dies off and institutional narrative-curation activity increases. Accessibility collapse is moderate (0.35) — alternative land-use decision inputs (economic, infrastructural) were never actually foreclosed by the stone under this reading, because the stone never functioned as the operative constraint in the first place. Resistance is moderate (0.4): descendants did not organize against the stone, they simply built without reference to it — a decay-by-inattention pattern rather than active repudiation.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter/beneficiary seats (tourism bodies, national disaster-education curators), the stone reads as a vindicated coordination mechanism — proof ancestral wisdom works. From the payer seats (residents who settled below the line, and their descendants), the same object was never consulted as an input to settlement decisions at all; its presence or absence made no difference to where they built. The engine should compute these as structurally different experiences of the same artifact: one seat receives narrative capital from an artifact of decayed function, the other seat bears the settlement-pattern risk the artifact was originally meant to prevent, without the artifact having actually constrained anyone's behavior.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (tourism/heritage bodies, national narrative curators) sit near the full-beneficiary end of directionality: they collect reputational, educational-program, and visitor-economy value from the stone's mythic status, and bear none of the cost if the underlying coordination claim is false. Payers (low-lying residents, future coastal households) sit near the full-target end: they bear the actual settlement risk the stone was meant to mitigate, and under this reading receive no behavioral guidance from it in return. Municipal planners are treated as agenda_setter but with constrained exit — they operate the actual land-use machinery and cannot simply defer to a symbolic marker even if they wanted institutional cover for hazard decisions.
 *
 * MANDATROPHY ANALYSIS:
 *   The stone's founding problem — generational amnesia about tsunami hazard causing repeated resettlement in vulnerable zones — is here judged dead as an actively-solved problem via the stone itself: the problem persists in the world (people still forget, still build low), but the stone no longer functions as the solution mechanism; it has been repurposed into a commemorative object serving a different institutional function (heritage tourism, national disaster mythology). Classifying this as piton rather than snare matters: no single party is coercively extracting from residents through active enforcement of the stone's directive (there is none), but the diffuse cost of a false 'we already have a solution' narrative falls on future households who might otherwise demand harder land-use rules. Reading this as tangled_rope would wrongly imply active enforcement machinery exists; reading it as mountain would wrongly imply the stone's authority is natural law rather than an artifact whose behavioral force could have persisted or lapsed as an empirical matter — which is exactly the question this kernel decomposition exists to separate from its sibling reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stone_efficacy_evidentiary_gap,
    'Is there any surviving record — oral history, land deed, municipal document — establishing whether residents who built below the stone''s line in the decades before 2011 were aware of the inscription and consciously disregarded it, versus simply never having encountered it as an operative consideration?',
    'Oral history interviews with elderly Aneyoshi residents and their descendants; review of pre-2011 municipal land-allocation records for any reference to the stone; comparison with other Tohoku tsunami-stone communities where compliance behavior is better documented.',
    'If residents demonstrably knew of and consciously ignored the stone, this reading strengthens (decayed observance, not mere unfamiliarity). If residents genuinely never encountered the stone as a live consideration, the constraint may be better characterized as having no coordination function to decay from at all — closer to a pure mountain-of-memory artifact than a decayed rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stone_efficacy_evidentiary_gap, empirical, 'Whether decay-of-observance or original-non-encounter better describes the residents'' relationship to the stone.').

omega_variable(
    kernel_framing_disambiguation,
    'Is the Aneyoshi stone''s ''behavioral force'' a single empirical fact that this reading and its sibling disagree about, or are the two readings measuring genuinely different observables (e.g., ''was the stone consulted'' vs. ''did settlement patterns happen to track the stone''s line for unrelated reasons'')?',
    'Settlement-pattern GIS analysis correlating build-date, elevation, and distance from the stone''s marked line against population/land-price data, to distinguish coincidental correlation with the hazard line from actual behavioral consultation of the inscription.',
    'If settlement patterns tracked the line for reasons unrelated to the stone (e.g., all low land was simply less desirable/more flood-prone for unrelated infrastructure reasons), that would support the commemorative_husk reading''s claim that the stone was not the operative mechanism even where geography happened to align with its directive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_disambiguation, conceptual, 'Whether the two sibling readings disagree about a fact or are indexing different observables under one label.').

omega_variable(
    survivorship_narrative_beneficiary_incentive,
    'To what extent do national disaster-education programs and tourism bodies have an incentive to over-attribute 2011 survival to ancestral stone-wisdom rather than to elevation and infrastructure factors, and does this incentive bias the historical record available for adjudicating the kernel contest?',
    'Content analysis of disaster-education curricula and tourism materials pre- and post-2011 for changes in how the stone is described; comparison with academic disaster-risk literature''s treatment of the same case.',
    'If beneficiary-institution narratives dominate the available public record, the apparent evidentiary balance in favor of behavioral_competence_reading may be an artifact of institutional narrative capture rather than genuine historical fact — reinforcing rather than resolving the kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survivorship_narrative_beneficiary_incentive, conceptual, 'Whether beneficiary incentives have shaped the available historical record adjudicating the two readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__commemorative_husk_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(aney_tr_t13, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 13, 0.22).
narrative_ontology:measurement(aney_tr_t26, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 26, 0.38).
narrative_ontology:measurement(aney_tr_t39, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 39, 0.52).
narrative_ontology:measurement(aney_tr_t52, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 52, 0.63).
narrative_ontology:measurement(aney_tr_t65, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 65, 0.73).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 78, 0.82).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(aney_be_t13, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 13, 0.25).
narrative_ontology:measurement(aney_be_t26, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 26, 0.38).
narrative_ontology:measurement(aney_be_t39, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 39, 0.5).
narrative_ontology:measurement(aney_be_t52, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 52, 0.6).
narrative_ontology:measurement(aney_be_t65, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 65, 0.66).
narrative_ontology:measurement(aney_be_t78, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 78, 0.71).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_commitment__commemorative_husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__commemorative_husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_commitment__commemorative_husk_reading, 0.06).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% This story and aneyoshi_stone_commitment__behavioral_competence_reading are sibling readings of the same kernel (a single 1933 warning stone in Aneyoshi, Japan) under the ε-invariance principle. This reading (commemorative_husk_reading) authors ε=0.71, treating the stone's coordination function as decayed by 2011, with survival attributed to elevation/infrastructure rather than compliance, and the stone's present function as symbolic-capital generation for tourism and national disaster-education narrative curators. The sibling reading (behavioral_competence_reading) would author a low ε, treating the stone as a functioning 78-year land-use rule that genuinely constrained settlement and thereby produced the observed survival pattern. Both stories describe the same physical artifact and the same historical interval, but assert structurally incompatible claims about whether a coordination mechanism was operative — hence two constraint stories, not one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
