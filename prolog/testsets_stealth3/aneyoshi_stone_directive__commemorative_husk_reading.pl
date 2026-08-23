% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
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
 *   An inscribed stone marker stands above a Sanriku coastal hamlet, carrying
 *   an instruction from post-surge rebuilders not to site dwellings below the
 *   marked line. This story instantiates the commemorative_husk_reading of
 *   that arrangement: the instruction's behavioral force was carried by the
 *   founding catastrophe generation, lapsed as that generation died across
 *   the middle decades of the 1933-2011 inter-catastrophe period, and left
 *   the stone as a maintained memorial whose residual customary weight
 *   continued to chill economically rational use of the lower terraces
 *   without any enforcement machinery behind it. The reading's high
 *   extraction assessment reflects opportunity costs borne against a
 *   protective service this reading declines to attribute to the inscription;
 *   the reading's beneficiary structure is asymmetric in time — payers bore
 *   costs throughout the interval, while the seats positioned to gain from
 *   the husk's decay (development interests) and from its afterlife (memory
 *   institutions) arrived at the margins. KEY AGENTS (by structural
 *   relationship): - municipal_authority: Agenda-setting custodian
 *   (institutional/arbitrage) — administers the stone as cultural property
 *   and references the line in planning; could retire the line's standing but
 *   never has - founding_generation_elders: Original agenda-setters
 *   (moderate/identity_locked) — carried the instruction by living witness;
 *   their attrition is the decay mechanism - disaster_memory_institutions:
 *   Late-arriving beneficiary (organized/mobile) — curate the stone's
 *   commemorative afterlife - village_descendant_community: Dual-positioned
 *   seat (moderate/identity_locked) — receives memorial continuity, forgoes
 *   lower-terrace siting - coastal_fishing_households: Primary payer
 *   (powerless/constrained) — bear the siting penalty on the ground nearest
 *   the fishery - coastal_development_interests: Secondary payer positioned
 *   to gain from decay (organized/mobile) — proposals chilled during the
 *   interval, barriers falling as salience fades - hazard_mappers: Excluded
 *   voice (institutional/mobile) — computed-hazard authority outside the
 *   commemorative frame - disaster_anthropology_researchers: Analytical
 *   observer (analytical/analytical) — the seat where the kernel contest is
 *   conducted
 *
 * KEY AGENTS:
 *   - municipal_authority: Agenda-setting custodian (institutional/arbitrage) — administers the stone and its planning references; could retire the line's standing but never has
 *   - founding_generation_elders: Original agenda-setters (moderate/identity_locked) — carried the instruction by living witness; their attrition is the decay mechanism
 *   - disaster_memory_institutions: Late-arriving beneficiary (organized/mobile) — curate the commemorative afterlife that emerged after 2011
 *   - village_descendant_community: Dual-positioned seat (moderate/identity_locked) — receives memorial continuity while forgoing lower-terrace siting
 *   - coastal_fishing_households: Primary payer (powerless/constrained) — bear the siting penalty on the ground nearest the fishery
 *   - coastal_development_interests: Secondary payer positioned to gain from decay (organized/mobile) — chilled proposals during the interval, falling barriers as salience fades
 *   - hazard_mappers: Excluded voice (institutional/mobile) — computed-hazard authority outside the commemorative frame
 *   - disaster_anthropology_researchers: Analytical observer (analytical/analytical) — conducts the behavioral-efficacy contest between the readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, 0.76).
domain_priors:suppression_score(aneyoshi_stone_directive__commemorative_husk_reading, 0.15).
domain_priors:theater_ratio(aneyoshi_stone_directive__commemorative_husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_directive__commemorative_husk_reading, "Aneyoshi Stone Directive — Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_stone_directive__commemorative_husk_reading, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__commemorative_husk_reading, '9d90dcee-facd-4629-bb5c-89ab48b257f2').
narrative_ontology:cs_kernel_codification('9d90dcee-facd-4629-bb5c-89ab48b257f2', fixed_text).
narrative_ontology:cs_authority_grounding('9d90dcee-facd-4629-bb5c-89ab48b257f2', lineage).
narrative_ontology:cs_interpretation_layer_present('9d90dcee-facd-4629-bb5c-89ab48b257f2').
narrative_ontology:cs_reading_relation('9d90dcee-facd-4629-bb5c-89ab48b257f2', aneyoshi_stone_directive__behavioral_competence_reading, forecloses).
narrative_ontology:cs_axiom('9d90dcee-facd-4629-bb5c-89ab48b257f2', foundational, directive_behavioral_force_extinct).
narrative_ontology:cs_axiom_status(directive_behavioral_force_extinct, holdable).
narrative_ontology:cs_axiom_grounding('9d90dcee-facd-4629-bb5c-89ab48b257f2', directive_behavioral_force_extinct, empirically_contingent).
narrative_ontology:cs_axiom('9d90dcee-facd-4629-bb5c-89ab48b257f2', foundational, commemorative_persistence_without_compliance).
narrative_ontology:cs_axiom_status(commemorative_persistence_without_compliance, holdable).
narrative_ontology:cs_axiom_grounding('9d90dcee-facd-4629-bb5c-89ab48b257f2', commemorative_persistence_without_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('9d90dcee-facd-4629-bb5c-89ab48b257f2', lived_memory_transmitted_ordinance).
narrative_ontology:cs_drift_state('9d90dcee-facd-4629-bb5c-89ab48b257f2', inter_catastrophe_generation_turnover, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('9d90dcee-facd-4629-bb5c-89ab48b257f2', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, disaster_memory_institutions).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, village_descendant_community).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, coastal_fishing_households).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, founding_generation_elders).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, village_descendant_community).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__commemorative_husk_reading, catastrophe_memory_preservation_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the hamlet's civil affairs and holds the stone as a designated cultural property. Its land-use guidance and reconstruction notices have referenced the inscribed line across the period. It could reclassify the stone as purely commemorative and strike the line from planning references, but has never moved to do so; after 2011 the stone's fame made any such move politically untenable. It collects prestige and narrative continuity from custody of the stone, not payments from anyone.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, municipal_authority, agenda_setter,
    institutional, generational, arbitrage, regional).

% Survivors of the earlier surges who rebuilt the hamlet above the line and carried the instruction by word of mouth: correcting siting choices, telling the surge stories at gatherings, treating the stone as the written form of an obligation they personally witnessed. Their standing in the hamlet rested on that witness. As they aged and died across the middle decades of the period, the instruction lost its carriers.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, founding_generation_elders, agenda_setter,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__commemorative_husk_reading, founding_generation_elders, beneficiary).

% Municipal museums, heritage boards, and later disaster-education centers that curate the stone: plaques, school visits, documentary coverage, anniversary observances. Their programming depends on the stone remaining famous and legible, and they rose to prominence only after the 2011 surge made the stone internationally known; in the hamlet's earlier decades they had no presence.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, disaster_memory_institutions, beneficiary,
    organized, generational, mobile, national).

% The hamlet's households, descendants of the families who rebuilt above the line after the earlier surges. They tend the stone, join the observances, and draw continuity and mourning practice from it. Several households also forgo siting new construction or fish-processing sheds on the lower terrace, where the ground is level and closest to the landing, because the inscribed line and family habit weigh on siting choices. Leaving the hamlet would mean leaving the graves, the commons, and the family standing the memorial anchors.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, village_descendant_community, beneficiary,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__commemorative_husk_reading, village_descendant_community, payer).

% Work the ria fishery from the landing below the terraces. Level, road-adjacent ground near the water is the efficient site for gear sheds, ice, and housing, and the inscribed line crosses exactly that ground. Building below it invited elder censure in the early decades and, later, simple disapproval; some households lease or share space above the line at added cost instead. Moving to another port means selling boats and losing crew networks.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, coastal_fishing_households, payer,
    powerless, biographical, constrained, local).

% Contractors, aquaculture operators, and outside investors who periodically propose marinas, processing plants, or second-home clusters on the lower coastal terraces. Proposals stall in consultation when the inscribed line is cited, and several sponsors redirected capital to other stretches of the Sanriku coast where no comparable marker carries weight. Each decade of the stone's fading salience lowered the barrier their proposals faced.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests, payer,
    organized, biographical, mobile, regional).

% National survey and seismology agencies producing inundation maps, run-up models, and warning protocols. Their lines are computed from bathymetry and rupture scenarios rather than inherited inscriptions, and they fall partly outside and partly across the stone's line. They take part in official hazard planning but not in the commemorative observances or heritage designations where the stone's authority is reproduced.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, hazard_mappers, excluded,
    institutional, generational, mobile, national).

% Field researchers and historians studying whether the Sanriku stones governed settlement behavior or memorialize without commanding. They interview descendants, archive permits and plats, compare hamlets with and without heeded markers, and publish the comparisons on which the competing accounts of the stone rest.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, disaster_anthropology_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_directive__commemorative_husk_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_directive__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds the memory of a measured inundation boundary in fixed public form so that post-survivor generations rebuild above the reach of prior surges without needing written records or technical staff; and gives the hamlet a shared site and calendar for mourning the surge dead.
% TRANSFER_FUNCTION: Moves siting discretion on the lower terraces away from fishing households and prospective developers and toward an inscribed ancestral line; the forgone locational value accrues to no seat and dissipates, while commemorative attention, visitation, and heritage resourcing flow to the stone's custodians, increasingly after 2011.
% ABSENT_VOICES: Computed-hazard voices — the survey and seismology agencies — sit outside the commemorative frame in which the stone's line is reproduced, and the prospective builders whose stalled proposals effectively paid for the line's salience were never seated in any observance or designation decision; both would contest the line's standing if the conversation reached them.
% DISAPPEARANCE_RATIONALE: Custodial seats answer that arrangements depend on the stone: the hamlet's mourning calendar, the heritage programming, and the post-2011 narrative of ancestral foresight would lose their anchor overnight. Payer and technical seats answer that little would change on the ground: under this account the instruction already ceased to guide siting decades before 2011, so its removal would alter plaques and processions, not building placements. The dispute between those answers is the same dispute that separates this reading from its sibling.
% FOUNDING_PROBLEM: After the 1896 Meiji-era surge destroyed the Sanriku coastal villages — and again after 1933 — surviving hamlets needed a way to make rebuilt settlements avoid the inundation zone in a landscape of scattered ravine communities, limited literacy, and no technical planning staff; carved stones placed at the observed reach of the water encoded the kill line in the most durable medium available.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: national survey and seismology practice now supplies computed inundation lines, seawall standards, and warning protocols that perform the transmission work the inscription performed, and the disaster-anthropology literature treats the stones' siting function as superseded. The custodial seats attest the opposite — that keeping settlement off the line is a perennial need — and no seat outside the beneficiary set joins that attestation.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__commemorative_husk_reading, contested).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__commemorative_husk_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored high (0.76 at interval end) because under this reading the arrangement's operative content in the late period is forgone locational value on the lower terraces with no protective service attributable to the inscription — where protection occurred, this reading attributes it to topography and settlement luck, so the deference the line still commanded bought nothing the reading can credit to it. Suppression is authored low (0.15) and is a raw structural input, unscaled by power or scope: the husk coerces almost nothing directly; its chill is customary residue, and the suppression_requirement series traces the informal enforcement machinery (elder correction, community censure) dying across the period — a falling trajectory modeling enforcement decay, which is the sanctioned use of that series. Theater ratio is authored high (0.82): by interval end the stone's active life consists of observances, plaques, school visits, and heritage interpretation — performance surrounding an instruction nobody follows. Accessibility collapse is low (0.25): understanding the stone as memorial opens the lower terraces rather than closing them, unlike a natural law whose recognition eliminates alternatives. Resistance is low (0.15): no seat organizes to retire a memorial credited with saving the village, and the fishing households who bear the siting penalty are dispersed across hamlets without a coalition vehicle — a latent coalition possibility the record never realized. All three series run on one shared seven-point grid (0, 13, 26, 39, 52, 65, 78 years after 1933) so every metric is authored at every examined time point; the roughly thirteen-year spacing tracks generational turnover, the mechanism the reading holds responsible for the decay.
 *
 * PERSPECTIVAL GAP:
 *   The custodial seats and the payer seats should compute different types from identical structural data. From the municipal and memory-institution positions the arrangement is a living institution they administer and program against — mourning calendar, heritage designation, civic narrative — and its persistence looks like stewardship. From the fishing households' position the same arrangement is a dead letter that has priced their siting for decades without delivering anything they can attribute to it. The descendant community straddles the gap internally: memorial continuity received against terrace value forgone, which is why it is authored dual-role and why its directionality should land near symmetric. The researcher seat sees the contest itself as the finding. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (disaster_memory_institutions, village_descendant_community) derive low directionality — the husk subsidizes them with narrative material and commemorative focus. Declared victims (coastal_fishing_households, coastal_development_interests) derive high directionality — they bear the siting penalty, with the fishing households nearer the full-target end because their exit is livelihood-bound while development capital can redeploy along other stretches of coast. The descendant community's dual declaration (beneficiary with secondary payer role) should land it near symmetric: memorial receipts against forgone terrace value. The municipal authority sits as agenda-setter with arbitrage-grade administrative exit — near the beneficiary end structurally, yet it receives prestige and narrative continuity rather than the forgone land value, which is why the receipt surface is authored diffuse. No directionality overrides are used: the declarations are clean, and the one genuinely ambiguous seat (the dual-role hamlet community) is flagged in an omega rather than corrected by fiat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — transmitting a kill line to illiterate, unplanned, scattered post-surge settlements — died with computed hazard mapping, engineered seawalls, and national warning protocols. The arrangement persists anyway, maintained as commemoration. Reading the stone as rope would credit a dead mandate with live protective work, which is precisely the sibling reading's exposure; reading it as snare would demand a capturer the record does not show, because the forgone locational value dissipates rather than accruing to any seat. The husk reading resolves the mandatrophy question toward piton: mandate dead, form maintained, no curator of the directive itself — the curators arrived for the memorial, after the function was gone, which is why the beneficiary declarations attach to the interval's end state rather than its body. The cost-to-fix is prohibitive only in legitimacy terms: retiring an ancestral memorial credited with saving the village is untouchable politics, which is the cost-asymmetry signature of the type, not evidence of capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the commemorative_husk_reading of the aneyoshi_stone_directive kernel; what structural differences would adopting the sibling behavioral_competence_reading produce?',
    'Adjudicate the behavioral-efficacy dispute directly (see survival_attribution_dispute); whichever reading prevails re-authors epsilon, the victim set, and the type over the same referent.',
    'Under the sibling reading the instruction delivered protective coordination throughout the interval, epsilon falls sharply, the fishing households and development interests become net beneficiaries of a rope-like arrangement, and this file''s piton claim collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer bookkeeping: one reading of a two-reading kernel; sibling adoption inverts the structural assessment.').

omega_variable(
    survival_attribution_dispute,
    'Did the hamlet''s survival of the 2011 surge evidence the instruction''s behavioral force, or is the outcome overdetermined by ravine topography, terrace elevation, and settlement luck?',
    'Comparative analysis of Sanriku coastal hamlets controlling for elevation, aspect, and harbor orientation: did stone-heeding settlements outperform matched non-heeding settlements in 2011?',
    'Attribution to the instruction revives the sibling reading and collapses this file''s high epsilon; attribution to topography confirms the husk account and licenses the authored extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_attribution_dispute, empirical, 'Where the kernel contest is located: the causal credit for the 2011 outcome.').

omega_variable(
    husk_vs_dormant_cycle,
    'Is the arrangement a completed husk, or a dormant constraint that revives with each catastrophe''s memory cycle?',
    'Siting-deference and compliance series across the post-1896, post-1933, and post-2011 windows: revival spikes would show the instruction re-acquiring force after each event.',
    'A revival pattern reclassifies post-catastrophe windows as rope-like phases inside an oscillating lifecycle rather than terminal decay; the authored monotonic series would need cyclical re-authoring with 8-10 points per cycle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_vs_dormant_cycle, empirical, 'Terminal-decay versus oscillating-revival lifecycle.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the residual chill on lower-terrace construction structural (planning references and heritage designations citing the line) or internalized (deference habit persisting after the enforcing generation died)?',
    'Post-decay trajectory: if siting deference persists where no municipal document references the line, the internalized share dominates.',
    'Internalized suppression travels with the payers after any formal retirement of the line, raising effective suppression above the structural measure; purely structural chill would vanish with a planning-reference edit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized residual suppression mechanism.').

omega_variable(
    post_hoc_curator_benefit_timing,
    'Do the memory institutions'' benefits contaminate the inter-catastrophe assessment, given that they entered only after 2011 made the stone famous?',
    'Separate the ledger: custodial receipts (visitation, grants, programming) against the suppression-era opportunity costs borne before any curator existed.',
    'If custodial benefit is confined to the husk''s afterlife, the interval body remains a no-capturer arrangement and the piton reading stands; if custodial interests demonstrably shaped late-period maintenance of the line''s planning salience, the late interval acquires a capturer and drifts toward capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_hoc_curator_benefit_timing, conceptual, 'Whether late-arriving beneficiaries retroactively capture the husk.').

omega_variable(
    decay_transfer_direction,
    'Does decay of the line''s salience actually transfer usable ground to development interests, or did the post-2011 policy response re-harden the coastal exclusion beyond anything the inscription achieved?',
    'Compare pre-2011 proposal outcomes with post-2011 zoning, buy-up, and restricted-zone maps for the same terraces.',
    'If post-2011 regulation locked the coast harder than the stone ever did, the development-gains-from-decay dynamic reverses after the interval end and the payer seats'' forward-looking position flips; the authored interval closes before that reversal completes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decay_transfer_direction, empirical, 'Direction of value transfer as the husk decays.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__commemorative_husk_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_husk_tr_t0, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(aneyoshi_husk_tr_t13, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 13, 0.22).
narrative_ontology:measurement(aneyoshi_husk_tr_t26, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 26, 0.36).
narrative_ontology:measurement(aneyoshi_husk_tr_t39, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 39, 0.5).
narrative_ontology:measurement(aneyoshi_husk_tr_t52, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 52, 0.61).
narrative_ontology:measurement(aneyoshi_husk_tr_t65, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 65, 0.72).
narrative_ontology:measurement(aneyoshi_husk_tr_t78, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 78, 0.82).

% Extraction over time
narrative_ontology:measurement(aneyoshi_husk_be_t0, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(aneyoshi_husk_be_t13, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 13, 0.41).
narrative_ontology:measurement(aneyoshi_husk_be_t26, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 26, 0.53).
narrative_ontology:measurement(aneyoshi_husk_be_t39, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 39, 0.62).
narrative_ontology:measurement(aneyoshi_husk_be_t52, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 52, 0.68).
narrative_ontology:measurement(aneyoshi_husk_be_t65, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 65, 0.73).
narrative_ontology:measurement(aneyoshi_husk_be_t78, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 78, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_husk_su_t0, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(aneyoshi_husk_su_t13, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 13, 0.4).
narrative_ontology:measurement(aneyoshi_husk_su_t26, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 26, 0.31).
narrative_ontology:measurement(aneyoshi_husk_su_t39, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 39, 0.22).
narrative_ontology:measurement(aneyoshi_husk_su_t52, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 52, 0.14).
narrative_ontology:measurement(aneyoshi_husk_su_t65, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 65, 0.09).
narrative_ontology:measurement(aneyoshi_husk_su_t78, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 78, 0.06).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__commemorative_husk_reading, identity_coordination).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Aneyoshi tsunami stone' covers two structurally distinct claims about one artifact: that its instruction bound settlement behavior continuously from 1933 to 2011, and that its behavioral force lapsed mid-period leaving a commemorated husk with residual customary chill. The claims license different epsilon over the same referent, so per the epsilon-invariance principle they are authored as separate stories — this file and aneyoshi_stone_directive__behavioral_competence_reading — and linked here. The sibling is the upstream claim in public narrative (higher ambient confidence, cited as evidence of ancestral foresight); this reading is the skeptical downstream decomposition whose high epsilon exists only if the upstream attribution fails.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
