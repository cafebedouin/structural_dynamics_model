% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_preservation__survival_competence_reading
 *   human_readable: Catastrophe-Memory Ritual Regime (Survival-Competence Reading)
 *   domain: religious studies / collective memory / ritual practice
 *
 * SUMMARY:
 *   River-delta settlements that survived a catastrophic flood-and-famine
 *   cycle maintain an obligatory annual observance straddling the disaster's
 *   anniversary: memorial liturgy for the dead, followed by route walks to
 *   high ground, cue-teaching (which odors precede the crest, which wells
 *   spoil first, which roads go under), inspection of seed and grain caches,
 *   and a relief-fund levy. Absence draws fines from the relief fund,
 *   exclusion from aid rosters and marriage brokerage, and reputational cost.
 *   On the reading instantiated here, the drill segments preserve real
 *   operational threat-recognition capacity that would otherwise decay across
 *   the generational memory gap, while the memorial envelope that carries it
 *   imposes heavy, unchosen costs on the living. The arrangement entangles
 *   grief with drill: the mourning practice supplies the emotional
 *   enforcement that keeps the training attended, and the training supplies
 *   the justification that keeps the mourning compulsory. KEY AGENTS (by
 *   structural relationship): - ritual_council_of_elders: Agenda-setter
 *   (organized/identity_locked) — administers the calendar, adjudicates
 *   compliance, collects precedence - descendant_generations: Designated
 *   beneficiary (powerless/trapped) — inherits the endowment without voice in
 *   its terms - observant_households: Primary payer with an insurance stake
 *   (moderate/constrained) — bears labor, levy, and lost workdays -
 *   ritual_dissenting_youth: Secondary payer (moderate/constrained) —
 *   complies under sanction without crediting the transfer -
 *   civil_defense_agency: Excluded institutional actor (institutional/mobile)
 *   — would formalize training to evidentiary standards -
 *   disaster_ethnographers: Analytical observer (analytical/analytical) —
 *   audit the transfer claim from outside
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, 0.64).
domain_priors:suppression_score(catastrophe_memory_preservation__survival_competence_reading, 0.62).
domain_priors:theater_ratio(catastrophe_memory_preservation__survival_competence_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, accessibility_collapse, 0.46).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__survival_competence_reading, "Catastrophe-Memory Ritual Regime (Survival-Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__survival_competence_reading, "religious studies / collective memory / ritual practice").

domain_priors:requires_active_enforcement(catastrophe_memory_preservation__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__survival_competence_reading, '7ea7b2db-b1e6-4423-ae78-c338cc5be32d').
narrative_ontology:cs_kernel_codification('7ea7b2db-b1e6-4423-ae78-c338cc5be32d', distributed).
narrative_ontology:cs_authority_grounding('7ea7b2db-b1e6-4423-ae78-c338cc5be32d', lineage).
narrative_ontology:cs_interpretation_layer_present('7ea7b2db-b1e6-4423-ae78-c338cc5be32d').
narrative_ontology:cs_reading_relation('7ea7b2db-b1e6-4423-ae78-c338cc5be32d', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ea7b2db-b1e6-4423-ae78-c338cc5be32d', catastrophe_memory_preservation__hybrid_atrophy_reading, influences).
narrative_ontology:cs_axiom('7ea7b2db-b1e6-4423-ae78-c338cc5be32d', foundational, ritual_drill_transfers_threat_competence).
narrative_ontology:cs_axiom_status(ritual_drill_transfers_threat_competence, holdable).
narrative_ontology:cs_axiom_grounding('7ea7b2db-b1e6-4423-ae78-c338cc5be32d', ritual_drill_transfers_threat_competence, empirically_contingent).
narrative_ontology:cs_axiom('7ea7b2db-b1e6-4423-ae78-c338cc5be32d', secondary, intergenerational_stewardship_obligation).
narrative_ontology:cs_axiom_status(intergenerational_stewardship_obligation, holdable).
narrative_ontology:cs_axiom_grounding('7ea7b2db-b1e6-4423-ae78-c338cc5be32d', intergenerational_stewardship_obligation, deontological).
narrative_ontology:cs_reference_frame('7ea7b2db-b1e6-4423-ae78-c338cc5be32d', founder_generation_full_transfer_baseline).
narrative_ontology:cs_drift_state('7ea7b2db-b1e6-4423-ae78-c338cc5be32d', contemporary_mitigation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7ea7b2db-b1e6-4423-ae78-c338cc5be32d', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, descendant_generations).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, ritual_council_of_elders).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, observant_households).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, ritual_dissenting_youth).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, observant_households).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__survival_competence_reading, embodied_memory_persistence_hypothesis).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__survival_competence_reading, rare_event_rehearsal_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Keeps the observance calendar, transmits the liturgy of the flood years, hears excuses, and disciplines absence through relief-fund fines and reputation. Office passes along lineage lines; the officeholders' standing in marriage brokerage and land matters rides on the observances continuing uninterrupted, and several have spent their entire adult lives learning and teaching the old signs. Stepping back from the calendar would unravel the authority the office confers.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ritual_council_of_elders, agenda_setter,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__survival_competence_reading, ritual_council_of_elders, beneficiary).

% Not yet born or still children in the settlements. Whatever the observances manage to carry — the smell of the river before it crests, which wells spoil first, the order and route of movement to high ground — arrives to them as inherited habit they never bargained for. They cannot attend, vote, or refuse the endowment being compiled on their behalf, and they bear the consequences of both its success and its neglect.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, descendant_generations, beneficiary,
    powerless, generational, trapped, regional).

% Host the anniversary table, close shops for the memorial days, walk the children through the old routes and signs, and absorb the food, labor, levies, and lost workdays. Their grandparents' names are on the memorial stones; leaving the observance would cost them kin ties, marriage standing, and burial plots. They also hold the practical payoff: in the last near-miss, the households that kept the drills moved to high ground well ahead of the official warning.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, observant_households, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__survival_competence_reading, observant_households, beneficiary).

% Comply minimally and often ironically. Many commute to work outside the settlements and count the memorial days as lost wages; some privately prepare with phone alerts and official maps instead. Skipping outright brings relief-fund fines and cold shoulders at market, so most show up, go through the motions, and leave early — paying the full cost of attendance while crediting none of its purpose.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ritual_dissenting_youth, payer,
    moderate, immediate, constrained, regional).

% Runs the levee district and the regional warning network two valleys over. Its staff have asked to audit the settlements' drill routes against flood models and to fold them into standardized preparedness certification; the council has declined, citing the observances' sufficiency. Compelling participation would require litigation the agency has not chosen to start, so it plans around the settlements rather than with them.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, civil_defense_agency, excluded,
    institutional, generational, mobile, national).

% Field researchers who have recorded the observances for two decades, timed the evacuation walks, and compared the transmitted cue-lists against hydrological records. They publish, advise, and testify when asked; they neither host nor enforce anything and hold no stake in the calendar continuing or stopping.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, disaster_ethnographers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__survival_competence_reading, ritual_council_of_elders).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Carries rare-event hazard knowledge across the generational memory gap by embedding cue-recognition and movement drills inside obligatory communal observance, so that each cohort rehearses responses to events too infrequent for individual learning to cover.
% TRANSFER_FUNCTION: Moves present-member time, labor, money, and behavioral freedom (trade, travel, and occupation on memorial days) into a maintenance program whose yield — recognition-and-response competence — accrues to future community members; a secondary flow of precedence and honor moves to the officeholders who run the calendar.
% ABSENT_VOICES: The descendant generations — the designated beneficiaries — are definitionally absent and cannot consent to or refuse the endowment. Dissenting youth are present but voteless in calendar governance. Neighboring settlements exposed to the same river have no seat at all. The civil defense agency stands wholly outside the frame and would subject the drill content to evidentiary review it has never been permitted to conduct.
% DISAPPEARANCE_RATIONALE: The grief calendar, the mutual-aid roster, the marriage-brokerage season, and the relief fund all hang off the observance cycle; overnight removal would force each to be rebuilt or dropped within a year, and the settlements would need some substitute vehicle — written manuals, agency drills, or nothing — to carry hazard knowledge past the oldest cohort's memory. The social world rearranges quickly; whether survival capacity rearranges for better or worse is exactly what the transfer-efficacy question remains open on.
% FOUNDING_PROBLEM: After the flood-and-famine years, the settlements found that children raised after the waters receded did not know the signs — which odors precede the crest, which wells spoil, which roads go under first — and the near-repeat generation nearly replicated the original death toll. The observance was built to move the survivors' knowledge into bodies and habits that would outlive the survivors.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: settlement archive ledgers recording casualty clustering against household drill-participation in the near-repeat event; the civil defense agency's own after-action memos crediting early self-evacuation ahead of official warnings; and university oral-history collections gathered independently of the council. The council's own attestations are set aside as interested. No party disputes that the founding problem existed; the dispute is whether mitigation has since retired it.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__survival_competence_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction sits at 0.64 at interval end: participation is costly (levies, closed shops, restricted travel on memorial days, weeks of preparatory labor), the burden predates every current payer, and a measurable slice of observance time produces no drill content. Suppression at 0.62 is a raw structural property, unscaled by power or scope — the fines, exclusions, and reputational machinery are what hold attendance up as voluntary belief wanes; the engine scales only extractiveness. Theater at 0.33: roughly a third of observance activity (memorial liturgy proper, processional protocol) is commemorative performance without operational payload, while two-thirds (route walks, cue-drills, cache audits) carries the transfer load. Accessibility_collapse at 0.46: alternatives exist — official maps, alert subscriptions, written manuals, the agency's certification courses — but the community's fidelity argument (written instructions lose the cues; alerts arrive after the signs) keeps them from substituting. Resistance at 0.52: youth disengagement, ironic minimal compliance, and agency pressure to standardize. Claim and metrics are independent authored facts: the tangled_rope claim states the structure as this reading sees it (genuine coordination function, asymmetric burden, active enforcement); the metrics describe operation as observed. The series run on one shared time grid (points 0, 8, 16, 24, 32, 40) so every tracked metric is authored at every examined point. The arrangement also oscillates annually — enforcement demand spikes in the anniversary season and remits between — so the series samples a fixed season each cycle; the plotted trend is therefore secular drift, not cycle phase, with the intra-year cycle documented here rather than in the grid.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the council seat the arrangement is a sacred trust whose continuity is the community's continuity; from the observant-household seat it is honored duty that also paid off in the last near-miss; from the dissenting-youth seat it is lost wages enforced by fines; from the descendant seat it is imperceptible — the designated beneficiary does not yet exist to perceive anything, the purest indexical hole in the story. The engine computes per-seat classifications from power, exit, and declared position; this commentary does not adjudicate which seat is right.
 *
 * DIRECTIONALITY LOGIC:
 *   Descendant generations are declared beneficiaries with no present cost-bearing and no exit: their derived directionality sits at the full-beneficiary end, a subsidy flowing backward in time from people who pay now to people who collect later. Observant households are payers with a declared secondary beneficiary position — insurance value, meaning, and the demonstrated payoff of drilled response — placing them mid-to-high on the target axis rather than at the extreme. Dissenting youth are payers with constrained exit and no credited benefit, sitting nearest the full-target end. The council performs officiating labor but collects precedence and controls the calendar; its identity_locked exit (the officeholders have become the observance) amplifies rather than offsets its beneficiary-side stake. No directionality overrides are declared: the beneficiary/victim declarations plus exit atoms already produce the intended relationships, and the agency and observer seats stand outside the derivation. Suppression remains unscaled in all of this; effective extraction is the engine's arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — competence decay across the generational memory gap after the catastrophe — is contested rather than dead: mitigation works have shortened return periods, yet the archive shows infrastructure failing during the last near-miss precisely where drilled households self-evacuated early. The status-times-verdict read (contested founding problem, world-rearranging disappearance) declines to certify a dead-mandate zombie: the arrangement still organizes real activity around a possibly-still-live problem, so mandatrophy_resolved is deliberately not declared. Classification prevents two symmetric mislabels: reading the regime as empty ceremony ignores the drill segments that demonstrably moved households early in the near-miss; reading it as pure dutiful coordination ignores that its costs fall on the unconsenting present under active sanction. The tangle — grief carrying drill, drill justifying grief — is the finding, and the atrophy question is routed to the omega set rather than settled here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_transfer_efficacy,
    'Does ritual participation actually produce measurable threat-recognition and response competence in participants, or does the settlements'' survival record come from co-present infrastructure (levees, warning network) that would protect them regardless?',
    'Comparative timing studies of evacuation walks against agency flood models, and retrospective coding of near-event responses that separates ritual-trained behaviors from infrastructure-mediated saves.',
    'If transfer is illusory, the arrangement''s coordination leg collapses and its costs read as ceremonial impost borne under sanction; if real, the same costs read substantially as the price of maintaining rare-event competence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_transfer_efficacy, empirical, 'Whether the drill content of the observance genuinely transfers operational capacity.').

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the survival_competence_reading of the catastrophe_memory_preservation kernel; how would classification shift under the mourning_practice_reading or hybrid_atrophy_reading siblings?',
    'Author the sibling files and compare computed types. The disagreement between readings is located at a single structural element: whether the drill segments of the observance transfer operational capacity across generations. The mourning reading denies it; the hybrid reading grants it historically and denies it presently; this reading affirms it as ongoing.',
    'Under mourning_practice_reading the victim set thins to present-autonomy only and extraction falls toward ceremonial levels; under hybrid_atrophy_reading theater climbs toward inertial-performance territory. Those are different constraints with different epsilon and different victim sets, linked as one family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this constraint is one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    cross_temporal_consent_legitimacy,
    'Is imposing present costs for the benefit of unconsulted future members a legitimate stewardship relation or an uncompensated taking from the living?',
    'Turns on the community''s own account of obligation to descendants and on whether present members would re-authorize the observances under fair deliberation that weighted non-voting seats.',
    'If read as taking, the burden on present payers weighs heavier and the tangle skews toward extraction; if read as stewardship, the identical costs read as duty and the coordination leg dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_temporal_consent_legitimacy, preference, 'Normative legitimacy of cross-temporal cost imposition on non-consenting parties.').

omega_variable(
    suppression_mechanism_split,
    'Is compliance sustained by structural sanction (relief-fund fines, exclusion from aid rosters and marriage brokerage) or by internalized identity (belief that absence invites misfortune upon the household)?',
    'Post-exit trajectory of emigrated members: whether fear of sanction and felt obligation persist after they leave the enforcement perimeter.',
    'If the internalized share is high, formal liberalization would not release the burden and measured suppression understates the grip; if structural, lifting fines and exclusions would lower it rapidly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized suppression mechanism in ritual compliance.').

omega_variable(
    hazard_regime_retirement,
    'Has engineered mitigation (levee completion, forecast accuracy) retired the flood-and-famine regime far enough that the preserved competence no longer pays its maintenance cost?',
    'Recurrence-interval analysis of residual hazard against the competence threshold the drills maintain, revisited as infrastructure ages and funding lapses.',
    'A retired regime pushes the founding problem toward dead and the arrangement toward mandate obsolescence; a live regime keeps the founding problem open and the maintenance rational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hazard_regime_retirement, empirical, 'Whether the founding hazard regime still obtains under modern mitigation.').

omega_variable(
    custodian_authority_capture,
    'Do the elders'' rulings track preservation of transmission quality or preservation of their own office''s standing — for instance, do lineage-connected households receive easier excuse adjudication?',
    'Compare excuse-adjudication outcomes for lineage-connected versus unconnected households across a decade of observance calendars.',
    'Authority-capture would concentrate receipt of the arrangement''s gains in the council seat and skew the structure toward enforced collection; even-handed adjudication supports the stewardship account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custodian_authority_capture, empirical, 'Whether custodian enforcement serves transmission or office preservation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__survival_competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(cata_tr_t8, observed).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement_basis(cata_tr_t16, observed).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement_basis(cata_tr_t24, observed).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 32, 0.3).
narrative_ontology:measurement_basis(cata_tr_t32, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement_basis(cata_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(cata_be_t8, observed).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement_basis(cata_be_t16, observed).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement_basis(cata_be_t24, observed).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement_basis(cata_be_t32, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement_basis(cata_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t8, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 8, 0.49).
narrative_ontology:measurement_basis(cata_su_t8, observed).
narrative_ontology:measurement(cata_su_t16, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement_basis(cata_su_t16, observed).
narrative_ontology:measurement(cata_su_t24, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement_basis(cata_su_t24, observed).
narrative_ontology:measurement(cata_su_t32, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 32, 0.59).
narrative_ontology:measurement_basis(cata_su_t32, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(cata_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__survival_competence_reading, attachment_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the ritual preserves catastrophe memory' conflates three structurally distinct claims, decomposed per the epsilon-invariance principle into a constraint family: survival_competence_reading (this file — ongoing operational transfer, tangled structure with high extraction), mourning_practice_reading (symbolic continuity only — low extraction), and hybrid_atrophy_reading (historically functional, presently atrophied — rising theater, inertial persistence). Epsilon differs across the family because each reading is a different arrangement with a different victim set, not one constraint viewed from angles. This reading supplies the reference condition (founder-generation full competence) that the hybrid reading's drift claim is indexed to, hence the influence edge; all family members are mutually linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
