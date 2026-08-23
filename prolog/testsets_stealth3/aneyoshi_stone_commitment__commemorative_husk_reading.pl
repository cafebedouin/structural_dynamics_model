% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: aneyoshi_stone_commitment__commemorative_husk_reading
 *   human_readable: Aneyoshi Tsunami Warning Tablets - Commemorative Husk Reading
 *   domain: disaster anthropology / commitment systems / temporal institutional analysis
 *
 * SUMMARY:
 *   Two stone tablets stand on the hillside above the Aneyoshi district of
 *   Miyako City, Iwate Prefecture, erected in 1934 after the Showa Sanriku
 *   tsunami and inscribed with the high-water marks of the 1896 and 1933
 *   waves together with a directive not to build homes below them. In March
 *   2011 the Tohoku tsunami climbed the valley to just below the tablets and
 *   the hamlet above survived. This story instantiates ONE reading of the
 *   contested kernel aneyoshi_stone_commitment: the commemorative-husk
 *   reading, which holds that the inscribed directive has been behaviorally
 *   inert for decades - that land-use and building-siting decisions in the
 *   district have been made through prefectural reconstruction programs,
 *   seawall engineering, mortgage finance, and family economics, with the
 *   tablets functioning as a memorial object, a tourism anchor, and a
 *   curriculum prop rather than an operative rule. On this reading the 2011
 *   survival is attributed to the valley's steep topography, the hamlet's
 *   elevated site, and a wave that stopped short - geography and fortune, not
 *   obedience. The epsilon referent is the standing arrangement under
 *   contest: the community's stone-centered hazard-commitment practice as it
 *   actually operates (maintenance funding, ceremonial calendar, heritage
 *   narration), assessed by this reading's own lights - resources and
 *   credence collected against a protective function this reading says is not
 *   delivered. The sibling reading
 *   (aneyoshi_stone_commitment__behavioral_competence_reading) shares this
 *   referent and authors a low epsilon consistent with a live coordination
 *   rule; the two stories form a constraint family linked by
 *   network.affects_constraints, and the epsilon gap between them is the
 *   measurement the pair exists to take. KEY AGENTS (by structural
 *   relationship): - aneyoshi_residents: primary bearers of costs and holders
 *   of the survival narrative (moderate / identity_locked) -
 *   miyako_municipal_government: agenda setter (institutional / mobile) -
 *   administers, restores, and narrates the site -
 *   miyako_municipal_taxpayers: diffuse payers (powerless / mobile) -
 *   heritage_tourism_operators: secondary beneficiaries (organized / mobile)
 *   - disaster_education_institutions: narrative beneficiaries (institutional
 *   / mobile) - coastal_reconstruction_planners: excluded actors
 *   (institutional / mobile) - hold actual siting authority, outside the
 *   framework - heritage_tourism_visitors: attention payers and experience
 *   beneficiaries (powerless / mobile) - disaster_anthropology_observers:
 *   analytical observers (analytical / analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, 0.6).
domain_priors:suppression_score(aneyoshi_stone_commitment__commemorative_husk_reading, 0.15).
domain_priors:theater_ratio(aneyoshi_stone_commitment__commemorative_husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_commitment__commemorative_husk_reading, "Aneyoshi Tsunami Warning Tablets - Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__commemorative_husk_reading, "disaster anthropology / commitment systems / temporal institutional analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__commemorative_husk_reading, 'c38df5ea-362a-45ea-83c9-8ea64c697d03').
narrative_ontology:cs_kernel_codification('c38df5ea-362a-45ea-83c9-8ea64c697d03', fixed_text).
narrative_ontology:cs_authority_grounding('c38df5ea-362a-45ea-83c9-8ea64c697d03', lineage).
narrative_ontology:cs_interpretation_layer_present('c38df5ea-362a-45ea-83c9-8ea64c697d03').
narrative_ontology:cs_reading_relation('c38df5ea-362a-45ea-83c9-8ea64c697d03', aneyoshi_stone_commitment__behavioral_competence_reading, forecloses).
narrative_ontology:cs_axiom('c38df5ea-362a-45ea-83c9-8ea64c697d03', foundational, stone_directive_behaviorally_inert).
narrative_ontology:cs_axiom_status(stone_directive_behaviorally_inert, holdable).
narrative_ontology:cs_axiom_grounding('c38df5ea-362a-45ea-83c9-8ea64c697d03', stone_directive_behaviorally_inert, empirically_contingent).
narrative_ontology:cs_axiom('c38df5ea-362a-45ea-83c9-8ea64c697d03', secondary, survival_explained_by_topography_and_luck).
narrative_ontology:cs_axiom_status(survival_explained_by_topography_and_luck, holdable).
narrative_ontology:cs_axiom_grounding('c38df5ea-362a-45ea-83c9-8ea64c697d03', survival_explained_by_topography_and_luck, empirically_contingent).
narrative_ontology:cs_reference_frame('c38df5ea-362a-45ea-83c9-8ea64c697d03', commemorative_memorial_artifact).
narrative_ontology:cs_drift_state('c38df5ea-362a-45ea-83c9-8ea64c697d03', post_2011_pilgrimage_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c38df5ea-362a-45ea-83c9-8ea64c697d03', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, miyako_municipal_government).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, heritage_tourism_operators).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_education_institutions).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_residents).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_residents).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, miyako_municipal_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, heritage_tourism_visitors).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, heritage_tourism_visitors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in the hamlet above the inscribed high-water line. Households contribute labor and donations to the annual memorial service, and the village association arranges cleaning and upkeep of the tablets. Many families trace their presence to ancestors who rebuilt after the earlier tsunamis, and remaining in Aneyoshi is bound up with that story; younger members who leave for work rarely return. Land and building decisions are made through prefectural reconstruction programs, mortgage lending, and family economics; the inscribed line is cited at ceremonies but not consulted when siting a house. Moving away would mean leaving the community and the family graves that anchor it.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_residents, payer,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_residents, beneficiary).

% Administers the site as a designated municipal heritage asset: funds periodic restoration of the tablets, maintains access paths and signage, organizes or permits the annual memorial gathering, and hosts visiting delegations and school groups. The site features in the city's disaster-education and tourism materials as evidence of a community that heeded its ancestors' warning. The council could de-designate the site or let upkeep lapse, but the stones anchor a story the city tells about itself, and grant lines for heritage preservation depend on active maintenance.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, miyako_municipal_government, agenda_setter,
    institutional, generational, mobile, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__commemorative_husk_reading, miyako_municipal_government, beneficiary).

% Fund the preservation budget and the ceremonial logistics through municipal taxes and heritage grant mechanisms. Few ever visit the hamlet; their connection to the site is indirect, through the city budget line and press coverage. They have no formal voice in how the site is interpreted.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, miyako_municipal_taxpayers, payer,
    powerless, biographical, mobile, regional).

% Run bus tours, guesthouses, and souvenir stalls that include the stones on tsunami-memory itineraries along the Sanriku coast. Their marketing leans on the story that the village survived because it obeyed the stones. If the site lost its fame, the itinerary item would be dropped within a season; nothing binds them to it.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, heritage_tourism_operators, beneficiary,
    organized, immediate, mobile, regional).

% Schools, disaster-preparedness centers, and museums incorporate the stones into curricula and exhibits as a parable of listening to ancestral warning. Teaching materials repeat the claim that the 2011 water stopped below the inscribed line because the village honored it. Updating the lesson to match current hazard science would require revising printed materials and retraining staff, which happens slowly if at all.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_education_institutions, beneficiary,
    institutional, generational, mobile, national).

% Prefecture and city engineers who drew the post-2011 reconstruction lines, seawall alignments, and designated hazard zones for the ria coast. Their siting decisions reference instrumental surveys, inundation simulations, and land-appropriation law; the stone tablets appear nowhere in their planning documents. They would have practical objections to treating an unengineered 1934 elevation mark as a safety boundary, but no channel connects their practice to the memorial framework.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, coastal_reconstruction_planners, excluded,
    institutional, generational, mobile, regional).

% Travelers, school groups, and study tours who visit the tablets, photograph them, and hear the survival story from guides and plaques. They spend travel money and time on the visit and leave with the narrative that an old stone kept a village safe. Their attention sustains the site's fame; individually they have no say in how the story is told.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, heritage_tourism_visitors, beneficiary,
    powerless, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__commemorative_husk_reading, heritage_tourism_visitors, payer).

% Researchers of disaster memory and vernacular hazard communication who study the tablets as artifacts of intergenerational risk transmission. They publish analyses of whether the inscriptions changed settlement patterns and of how the 2011 event was narrated afterward. Their work feeds the contest between the readings but binds them to no outcome.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_anthropology_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__commemorative_husk_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: As it operates now, the arrangement coordinates collective remembrance: it fixes an annual calendar (service, cleaning, school visits), gives the hamlet a shared origin story, and channels heritage visitation along the Sanriku coast. It no longer coordinates land use - siting is governed by prefectural hazard zoning, seawall alignment, and market finance.
% TRANSFER_FUNCTION: Moves preservation funding and ceremonial labor from municipal taxpayers and residents toward site upkeep and the memorial calendar; moves attention, travel spending, and curriculum time from visitors and students toward the survival narrative; moves heritage revenue and grant eligibility toward the municipality and local operators.
% ABSENT_VOICES: Coastal reconstruction planners and practicing hazard engineers are absent - the people whose instruments actually decide where building is allowed have no seat in the stone-tradition conversation, and the tradition has no channel into their documents. Also absent: the founding generation whose flood memory the tablets encode; only the inscription speaks for them, and it cannot answer questions.
% DISAPPEARANCE_RATIONALE: The memorial world rearranges within months: the annual service loses its anchor, school curricula and preparedness exhibits lose their parable, tour itineraries drop the stop, and the city loses a load-bearing piece of its self-narrative. The built environment does not move: zoning lines, seawall alignments, and insurance ratings were never computed from the tablets, so no dwelling sits differently. That asymmetry - total rearrangement of the observance economy, zero rearrangement of land use - is this reading's central observation.
% FOUNDING_PROBLEM: After the 1896 Meiji Sanriku tsunami killed roughly twenty-two thousand people along the Sanriku coast and the 1933 Showa tsunami killed thousands more, the surviving households of Aneyoshi needed a way to keep the flood's reach in memory when records burn and institutions turn over - a durable, locally enforceable line below which rebuilding would not occur.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Iwate Prefecture's hazard-map program, national inundation-simulation standards, and the engineered seawall design process now formally carry flood-memory and siting-safety functions, and none of those institutions references the tablets; post-2011 reconstruction documents for the district cite surveys and simulations, not the inscribed line. Municipal heritage officers attest the contrary - that the tradition remains instructive - but they administer the site and sit inside the benefiting set; no disinterested party attests that the founding problem persists in its original form.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.60: on this reading the arrangement collects real resources - municipal preservation funding, resident labor and donations, curriculum time, tourist attention - and converts them into a protective narrative the reading judges undelivered; the collection is diffuse and uncoerced, hence well below coercive-extraction range, but far above any residual information-transmission cost. Suppression is low (0.15): nothing enforces the husk; the residual pull is mostly internalized (identity fusion with the survival narrative) rather than structural, with the split carried by the omega identity_suppression_internalization. Theater ratio is high (0.82): the annual service, the plaques, the guided visits, and the exhibit presentation dominate the arrangement's activity, while its nominal function - directing where buildings go - is performed by other institutions entirely. Accessibility collapse is low (0.20): once the husk character is understood, alternatives (hazard maps, inundation simulation, engineered defenses, statutory zoning) are fully available and already carry the load. Resistance is low (0.10): a memorial attracts scholarly critique but no one organizes against it. The temporal series share one eight-point grid (1934-2025). Suppression_requirement is tracked because this story's core dynamic is enforcement decay: the directive's mid-century self-enforcement - elder authority and social sanction against building below the line - collapses toward zero as prefectural planning absorbs siting decisions; the scalar suppression (0.15) sits above the series endpoint because it includes the internalized residue the series does not measure. Base extractiveness rises across the interval, jumping after 2011 when global pilgrimage and the 'the stones saved us' narrative peaked; the mild post-2018 easing reflects attention normalization. The dynamics are monotonic decay, not cyclical, so no oscillation mechanism is claimed. Claim and metrics are independent: piton is claimed from structure - an atrophied former coordination device, administered by an actor that could de-commission it but bears little of its cost, with no seat capturing the gains - not tuned to the metric profile.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (miyako_municipal_government) should compute as a low-effective-extraction seat: from inside the heritage office the tablets are a treasured civic asset whose upkeep is stewardship, and the city's grant lines and identity narrative depend on the site staying framed as living tradition. The payer seats compute differently: municipal taxpayers fund a site most never see; residents donate labor to a calendar they inherited; visitors pay attention to a lesson they cannot audit against hazard science. The residents' seat is genuinely dual - they bear the upkeep costs and hold the memorial benefit - which is why their directionality is pinned by override rather than left to an ambiguous derivation. The excluded planner seat experiences no constraint at all: the framework's authority never reached their documents. The engine computes these divergences from the structural data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (municipal government, tourism operators, education institutions, residents-as-memorial-keepers) drive those seats toward the beneficiary end; victim declarations (residents-as-upkeep-payers, municipal taxpayers) drive them toward the target end. The derivation is ambiguous only for aneyoshi_residents, who appear on both sides; the directionality_overrides entry pins their power atom (moderate - the only seat at that atom in this story) to d=0.55, slightly target-side, reflecting that material costs flow out while the memorial good is real but does not offset them materially. heritage_tourism_visitors are dual-positioned (experience received, attention paid) and derive near-symmetric from their declarations. coastal_reconstruction_planners hold the excluded role: per the R3 ruling an authored absence stays commentary-grade, so no override is authored for that seat and the canonical fallback applies.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - preserving flood memory in a form that outlives institutional record-keeping - was solved by other means: official hazard maps, inundation simulation, engineered seawalls, and statutory zoning now carry what the tablets once carried alone. The mandate is dead; the arrangement persists as calendar, curriculum, and exhibit. Naming the type prevents two mislabels: reading the arrangement through the sibling's lens renders it a live coordination rule and misses the decay; reading it through a capture lens renders it a fraud economy and misses that no seat captures the gains - tourism revenue roughly recirculates into the performance, and the municipality's heritage capital is a budget justification, not a rent stream. The structurally true shape is inertia without capture: the administrator could change the arrangement, the costs fall mostly on others, and the cost of fixing (de-commissioning a beloved identity object, or reviving a directive that modern law has no channel to honor) exceeds any benefit its fixers perceive. The R5 mismatch - founding problem dead, yet the world would visibly rearrange around the stones' absence - is the expected zombie-flag signature for this reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_behavioral_coupling,
    'This constraint is one reading (commemorative_husk_reading) of the kernel aneyoshi_stone_commitment; the sibling reading behavioral_competence_reading asserts the opposite structural fact - sustained behavioral coupling between the inscribed line and building siting from 1934 through 2011. Which reading does the archival record support?',
    'Reconstruct siting decisions against the inscribed elevation: pre- and post-1933 building footprints, post-2011 reconstruction lot assignments, mortgage and subsidy records, and household interviews on whether the stone line entered any siting decision.',
    'Resolution toward behavioral competence collapses this story''s epsilon toward coordination-cost levels and forces reclassification away from the husk profile; confirmation of independence fixes the husk reading and shifts the burden to the sibling story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_behavioral_coupling, empirical, 'Kernel contest: whether the stone directive ever behaviorally coupled to land-use decisions.').

omega_variable(
    survival_attribution_ambiguity,
    'Was the 2011 survival of the hamlet above the tablets caused by design factors (elevated siting, the bay''s steep bathymetry, the small pre-existing seawall) or by luck (a wave whose run-up stopped marginally below the inscribed line)?',
    'Inundation modeling of the 2011 source under alternative rupture scenarios, comparing run-up distributions at the Aneyoshi valley mouth against the inscribed elevation.',
    'If luck dominates, even the retrospective protective narrative fails and the narrative-credence channel of epsilon strengthens; if design dominates, part of the arrangement''s claimed function was incidentally delivered and epsilon eases downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_attribution_ambiguity, empirical, 'Whether the 2011 survival reflects delivered protection or fortune.').

omega_variable(
    identity_suppression_internalization,
    'Is the residual pull of the stone commitment on residents structural (municipal programming, the memorial calendar, the tourism economy) or internalized (identity fused with the ancestors-obeyed-and-lived narrative)?',
    'Post-exit and post-defunding trajectories: attendance and donation patterns when ceremonies lose municipal funding; whether relocated descendants maintain the observance.',
    'If internalized, the arrangement''s effective hold exceeds its structural measure - removal costs are higher than budget lines suggest and the identity_locked exit computation stands; if structural, defunding would dissolve the husk quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_suppression_internalization, empirical, 'Structural versus internalized persistence mechanism for the husk.').

omega_variable(
    epsilon_referent_scoping,
    'Should epsilon''s referent be the inscribed directive alone (a dead letter that extracts almost nothing) or the standing memorial-veneration arrangement that has grown around it (which collects resources and credence against an undelivered protective function)?',
    'Conceptual: fix the referent by what the kernel contest is about - the community''s hazard-commitment practice, not the physical stones; confirm against how the sibling reading scopes its own low epsilon over the same referent.',
    'Narrow scoping drops epsilon toward 0.05 and renders the story a purely inert husk; the broad scoping authored here is what supports the 0.60 value and the inertia-without-performance profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_referent_scoping, conceptual, 'Referent under-determination: directive-only versus veneration-arrangement scoping.').

omega_variable(
    memorial_value_preference,
    'Do residents affirmatively value remembrance as such - such that part of what this reading scores as extraction is in fact a purchased good?',
    'Preference elicitation independent of the memorial calendar: willingness to fund upkeep absent the tourism frame and the survival narrative.',
    'Affirmation lowers net epsilon and softens the husk verdict toward benign neglect; rejection confirms the credence channel as pure overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_value_preference, preference, 'Whether remembrance value offsets the scored extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__commemorative_husk_reading, 1934, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_husk_tr_t1934, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1934, 0.1).
narrative_ontology:measurement_basis(aneyoshi_husk_tr_t1934, observed).
narrative_ontology:measurement(aneyoshi_husk_tr_t1950, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement_basis(aneyoshi_husk_tr_t1950, observed).
narrative_ontology:measurement(aneyoshi_husk_tr_t1968, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1968, 0.3).
narrative_ontology:measurement_basis(aneyoshi_husk_tr_t1968, observed).
narrative_ontology:measurement(aneyoshi_husk_tr_t1986, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1986, 0.5).
narrative_ontology:measurement_basis(aneyoshi_husk_tr_t1986, observed).
narrative_ontology:measurement(aneyoshi_husk_tr_t2004, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2004, 0.65).
narrative_ontology:measurement_basis(aneyoshi_husk_tr_t2004, observed).
narrative_ontology:measurement(aneyoshi_husk_tr_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2011, 0.8).
narrative_ontology:measurement_basis(aneyoshi_husk_tr_t2011, observed).
narrative_ontology:measurement(aneyoshi_husk_tr_t2018, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2018, 0.85).
narrative_ontology:measurement_basis(aneyoshi_husk_tr_t2018, observed).
narrative_ontology:measurement(aneyoshi_husk_tr_t2025, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2025, 0.82).
narrative_ontology:measurement_basis(aneyoshi_husk_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(aneyoshi_husk_be_t1934, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1934, 0.25).
narrative_ontology:measurement_basis(aneyoshi_husk_be_t1934, observed).
narrative_ontology:measurement(aneyoshi_husk_be_t1950, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement_basis(aneyoshi_husk_be_t1950, observed).
narrative_ontology:measurement(aneyoshi_husk_be_t1968, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1968, 0.38).
narrative_ontology:measurement_basis(aneyoshi_husk_be_t1968, observed).
narrative_ontology:measurement(aneyoshi_husk_be_t1986, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1986, 0.45).
narrative_ontology:measurement_basis(aneyoshi_husk_be_t1986, observed).
narrative_ontology:measurement(aneyoshi_husk_be_t2004, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2004, 0.52).
narrative_ontology:measurement_basis(aneyoshi_husk_be_t2004, observed).
narrative_ontology:measurement(aneyoshi_husk_be_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2011, 0.6).
narrative_ontology:measurement_basis(aneyoshi_husk_be_t2011, observed).
narrative_ontology:measurement(aneyoshi_husk_be_t2018, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2018, 0.63).
narrative_ontology:measurement_basis(aneyoshi_husk_be_t2018, observed).
narrative_ontology:measurement(aneyoshi_husk_be_t2025, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2025, 0.6).
narrative_ontology:measurement_basis(aneyoshi_husk_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_husk_su_t1934, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 1934, 0.35).
narrative_ontology:measurement_basis(aneyoshi_husk_su_t1934, observed).
narrative_ontology:measurement(aneyoshi_husk_su_t1950, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 1950, 0.28).
narrative_ontology:measurement_basis(aneyoshi_husk_su_t1950, observed).
narrative_ontology:measurement(aneyoshi_husk_su_t1968, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 1968, 0.18).
narrative_ontology:measurement_basis(aneyoshi_husk_su_t1968, observed).
narrative_ontology:measurement(aneyoshi_husk_su_t1986, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 1986, 0.1).
narrative_ontology:measurement_basis(aneyoshi_husk_su_t1986, observed).
narrative_ontology:measurement(aneyoshi_husk_su_t2004, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 2004, 0.06).
narrative_ontology:measurement_basis(aneyoshi_husk_su_t2004, observed).
narrative_ontology:measurement(aneyoshi_husk_su_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 2011, 0.04).
narrative_ontology:measurement_basis(aneyoshi_husk_su_t2011, observed).
narrative_ontology:measurement(aneyoshi_husk_su_t2018, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 2018, 0.03).
narrative_ontology:measurement_basis(aneyoshi_husk_su_t2018, observed).
narrative_ontology:measurement(aneyoshi_husk_su_t2025, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 2025, 0.02).
narrative_ontology:measurement_basis(aneyoshi_husk_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__commemorative_husk_reading, information_standard).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'the Aneyoshi stone warning' decomposes into two structurally distinct claims over one kernel. The behavioral_competence_reading treats the inscribed directive as a live intergenerational land-use rule (low epsilon, coordination-cost profile); this commemorative_husk_reading treats it as a decayed memorial arrangement that collects resources and credence against an undelivered protective function (epsilon 0.60, high theater). Same referent, different reading-indexed epsilon per OQ-26. Upstream/downstream: the sibling's claim is the one cited in heritage and education materials as evidence for the arrangement's value, so the sibling story influences this one's legitimacy conditions; this story's decay finding, if confirmed, erodes the sibling's evidentiary base. Each file links the other via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(aneyoshi_stone_commitment__commemorative_husk_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
