% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__consequence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__consequence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: gelassenheit_separation__consequence_reading
 *   human_readable: Gelassenheit Separation — Consequence Reading (Technology Judged by Effect on Community Practice)
 *   domain: religious/technology_governance/commitment_systems
 *
 * SUMMARY:
 *   In many Old Order Amish and conservative Mennonite communities, the
 *   Ordnung's technology rulings are reasoned not from a fixed list of
 *   forbidden objects nor from a principle of structural non-entanglement,
 *   but from an explicit, revisable test: does adopting this technology
 *   increase or decrease the density of face-to-face visiting, labor
 *   exchange, and physical presence that mutual aid and eldercare depend on?
 *   This produces fine-grained, sometimes counterintuitive distinctions — a
 *   shared phone shanty at the end of the lane is acceptable because it
 *   requires walking out to use it (preserving some friction and communal
 *   visibility), while a phone inside the home is not, because it would let
 *   members skip the walk-over visit entirely. The reading is genuinely
 *   low-extraction as coordination — it protects a real collective good
 *   (mutual aid networks that keep the elderly fed and the harvest brought
 *   in) — but it imposes real, unevenly distributed costs on members whose
 *   lives are structured around dispersion (off-farm work, distant kin)
 *   rather than local density.
 *
 * KEY AGENTS:
 *   - ordnungsleit_church_leadership: sets rulings case-by-case using the consequence test (institutional/identity_locked)
 *   - settled_church_community: primary beneficiary of preserved visiting density (organized/constrained)
 *   - elderly_and_homebound_members: most dependent beneficiary, no alternative support (powerless/trapped)
 *   - technologically_curious_youth: bears the cost of denied home connectivity (powerless/identity_locked)
 *   - off_farm_wage_workers: bears commercial friction cost (moderate/constrained)
 *   - members_with_dispersed_family: bears relational cost the local calculus does not weigh (powerless/trapped)
 *   - outside_researchers_and_church_historians: analytical observer of cross-district variation (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__consequence_reading, 0.18).
domain_priors:suppression_score(gelassenheit_separation__consequence_reading, 0.42).
domain_priors:theater_ratio(gelassenheit_separation__consequence_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__consequence_reading, rope).
narrative_ontology:human_readable(gelassenheit_separation__consequence_reading, "Gelassenheit Separation — Consequence Reading (Technology Judged by Effect on Community Practice)").
narrative_ontology:topic_domain(gelassenheit_separation__consequence_reading, "religious/technology_governance/commitment_systems").

domain_priors:requires_active_enforcement(gelassenheit_separation__consequence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__consequence_reading, 'a3ce7992-50f9-4bbb-9415-e86c82d1bb8a').
narrative_ontology:cs_kernel_codification('a3ce7992-50f9-4bbb-9415-e86c82d1bb8a', distributed).
narrative_ontology:cs_authority_grounding('a3ce7992-50f9-4bbb-9415-e86c82d1bb8a', practice).
narrative_ontology:cs_interpretation_layer_present('a3ce7992-50f9-4bbb-9415-e86c82d1bb8a').
narrative_ontology:cs_reading_relation('a3ce7992-50f9-4bbb-9415-e86c82d1bb8a', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3ce7992-50f9-4bbb-9415-e86c82d1bb8a', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_axiom('a3ce7992-50f9-4bbb-9415-e86c82d1bb8a', foundational, separation_is_measured_by_practice_effect).
narrative_ontology:cs_axiom_status(separation_is_measured_by_practice_effect, holdable).
narrative_ontology:cs_axiom_grounding('a3ce7992-50f9-4bbb-9415-e86c82d1bb8a', separation_is_measured_by_practice_effect, instrumental).
narrative_ontology:cs_axiom('a3ce7992-50f9-4bbb-9415-e86c82d1bb8a', secondary, visiting_density_is_the_load_bearing_practice).
narrative_ontology:cs_axiom_status(visiting_density_is_the_load_bearing_practice, holdable).
narrative_ontology:cs_axiom_grounding('a3ce7992-50f9-4bbb-9415-e86c82d1bb8a', visiting_density_is_the_load_bearing_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('a3ce7992-50f9-4bbb-9415-e86c82d1bb8a', ordnung_practice_based_reasoning).
narrative_ontology:cs_drift_state('a3ce7992-50f9-4bbb-9415-e86c82d1bb8a', post_rural_electrification_and_telephone_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('a3ce7992-50f9-4bbb-9415-e86c82d1bb8a', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__consequence_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, settled_church_community).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, elderly_and_homebound_members).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, extended_kin_networks).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, technologically_curious_youth).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, off_farm_wage_workers).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, members_with_dispersed_family).
narrative_ontology:constraint_vindicates(gelassenheit_separation__consequence_reading, visiting_sustains_community_cohesion).
narrative_ontology:constraint_vindicates(gelassenheit_separation__consequence_reading, geographic_rootedness_preserves_mutual_aid).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The bishop and ministers set and revise the Ordnung's technology rulings district by district, applying a consequentialist test: does this device increase visiting and mutual aid, or does it let people withdraw into private convenience? They permit a telephone in a barn (business calls, does not displace porch conversation) but forbid one in the house (would let a daughter skip the visit and just call). They hold real discretion — the rulings are not read off a text but reasoned case by case — and their own standing depends on being seen to reason well.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, ordnungsleit_church_leadership, agenda_setter,
    institutional, generational, identity_locked, regional).

% Farm families who rely on face-to-face visiting, barn-raisings, and harvest labor exchange for both economic survival and social meaning. The consequence-based rulings keep technology from eroding the density of visiting that mutual aid depends on. They experience the rules as protecting something they value, not as arbitrary restriction — though leaving the community means losing the entire mutual-aid network at once.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, settled_church_community, beneficiary,
    organized, generational, constrained, local).

% Members who can no longer travel depend entirely on being visited; they have no alternative support network outside the community. The consequence test directly protects the practice that keeps them fed, checked-on, and connected. They have essentially no exit and are the clearest beneficiaries of any rule that keeps visiting obligatory rather than optional.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, elderly_and_homebound_members, beneficiary,
    powerless, biographical, trapped, local).

% Relatives across nearby church districts whose relationships are maintained through visiting rather than calling. The consequence framework's willingness to allow a shared phone in a barn or shanty (for scheduling visits, not replacing them) keeps regional kin ties functional without collapsing them into remote contact.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, extended_kin_networks, beneficiary,
    moderate, generational, constrained, regional).

% Younger members who want a phone in the home, easier communication with peers outside the district, or tools their English neighbors use freely. Under the consequence test their desire for private, in-home connectivity is precisely what the rule is built to deny, because it is read as displacing visiting. Their exit option is formally open during Rumspringa but socially catastrophic — leaving means losing family, community, and often their entire adult identity.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, technologically_curious_youth, payer,
    powerless, biographical, identity_locked, local).

% Members who work construction crews or shops off the home farm and need faster coordination with employers and clients than the visiting-based framework accommodates. They bear real economic friction — lost jobs, slower bids, missed calls — because the consequence test weighs their commercial convenience against the risk that phones-in-homes will erode visiting, and generally rules against them for home use.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, off_farm_wage_workers, payer,
    moderate, biographical, constrained, regional).

% Members whose children or siblings settled in distant, sparser communities cannot easily convert visiting into a substitute; a restricted or barn-only phone means real relationships attenuate for lack of any permitted remote-contact tool. The consequence framework's local calculus (protect this district's visiting density) does not weigh their particular, less-visitable kinship structure.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, members_with_dispersed_family, payer,
    powerless, biographical, trapped, continental).

% Sociologists and denominational historians who study Ordnung rulings across districts, documenting how the same consequence-based reasoning produces different rulings for functionally similar devices depending on local visiting patterns. They can observe the reasoning's internal coherence and its costs to those it excludes without holding a stake in either outcome.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, outside_researchers_and_church_historians, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__consequence_reading, diffuse).
narrative_ontology:fixing_cost_class(gelassenheit_separation__consequence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, revisable standard for evaluating any new technology by a single question — does it increase or decrease face-to-face visiting and mutual aid — so that districts are not forced into an all-or-nothing stance toward every new device and can reason coherently about genuinely novel cases (the telephone, the tractor, eventually the car).
% TRANSFER_FUNCTION: Moves convenience and individual communicative autonomy away from members who would prefer private, in-home, or long-distance connectivity, toward the collective good of sustained visiting density, labor exchange, and eldercare — concentrated benefit to those already embedded in dense local networks, concentrated cost to those whose relationships or work are geographically or socially dispersed.
% ABSENT_VOICES: Members who left the church over exactly this kind of ruling are not present in the Ordnung's deliberation; their objections (that the rule cost them a career or a distant relationship, not just an inconvenience) are known anecdotally but do not carry weight in the ministers' consequentialist reasoning, which is conducted entirely by and for those who remain.
% DISAPPEARANCE_RATIONALE: If the consequence-based Ordnung rulings vanished overnight, phone and technology adoption would likely converge rapidly toward surrounding rural norms — visiting frequency would decline as calling substituted for travel, mutual aid coordination would shift from face-to-face to phone-based, and the elderly/homebound would lose their strongest claim on being physically visited. The mutual-aid economy would not disappear but would restructure around remote coordination, changing who bears the burden of eldercare and labor exchange.
% FOUNDING_PROBLEM: As surrounding rural infrastructure electrified and phones spread in the early-to-mid 20th century, communities needed a way to adopt genuinely useful tools (medical emergencies, farm business) without letting adoption cascade into the same individualizing, community-eroding pattern observed in mainstream rural society — a problem of selective adoption, not blanket rejection.
% FOUNDING_PROBLEM_CORROBORATION: Sociologists of Anabaptist communities (e.g. studies of Old Order Amish technology adoption by Kraybill, Nolt, and others outside the church) corroborate that visiting frequency and mutual-aid participation remain measurably higher in districts with stricter phone-in-home prohibitions, and that districts which relaxed the rule saw visiting decline within a generation — an outside empirical check on a genealogy the ministers themselves also assert, so the corroboration is not purely self-interested even though it is not fully independent of members' own reporting.
narrative_ontology:disappearance_verdict(gelassenheit_separation__consequence_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__consequence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__consequence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gelassenheit_separation__consequence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__consequence_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__consequence_reading_tests).
:- end_tests(gelassenheit_separation__consequence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18 at interval end) because the consequence test genuinely tracks a coordination good — visiting and mutual aid — rather than serving as cover for concentrated rent extraction; there is no agent collecting a transfer from these rulings the way an extractive gatekeeper would. Suppression is moderate (0.42) because enforcement is real (shunning and social pressure back the Ordnung) but is calibrated case-by-case rather than blanket, and exit exists formally (baptism is voluntary, Rumspringa exists) even though it is socially very costly. Theater ratio is low (0.12) because the rulings are substantively deliberated, not performative — ministers visibly reason about specific cases rather than reciting fixed prohibitions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (settled community, elderly, extended kin) are those whose lives are already structured around local density — the consequence test protects exactly the practice pattern they depend on, so their directionality sits toward the beneficiary end. Payers (technologically curious youth, off-farm workers, members with dispersed family) are those whose situation the local-visiting calculus does not fit — their costs are real but structurally invisible to a test calibrated on local visiting density, so their directionality sits toward the target end despite formally equal church membership.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (selective adoption without individualizing cascade) remains live by outside sociological corroboration, which is exactly what prevents this from being classified as pure inertial extraction (piton) or captured rent-seeking (snare): the mechanism still measurably produces its claimed effect (higher visiting density in stricter districts). The classification as a genuine (if imperfect) coordination arrangement rather than pure extraction depends on this corroboration holding; if outside researchers found no measurable visiting-density difference between strict and relaxed districts, the founding problem would be effectively dead and the rule would be better classified as inertial or captured.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consequence_reading_committer_structure,
    'This constraint is one reading (consequence_reading) of the contested gelassenheit_separation kernel; the sibling readings (artifact_reading: visible distinction from the English regardless of function; principle_reading: functional non-entanglement in worldly systems) would rule differently on the same telephone-in-barn or tractor-for-belt-power cases. Where exactly does the disagreement locate structurally?',
    'Compare actual Ordnung rulings across districts that self-describe as reasoning from each of the three frames: districts using artifact_reading logic ban devices for resembling worldly objects even when functionally isolated or visiting-neutral; districts using principle_reading logic permit devices that are functionally isolated even if they increase in-home use; only consequence_reading districts produce the specific barn/home split on telephones documented here. The disagreement is located in what counts as the relevant harm — appearance (artifact), entanglement (principle), or practice-erosion (consequence) — not in disagreement about the facts of any given device.',
    'If a community''s actual rulings track appearance or entanglement rather than measured effect on visiting, this story misattributes the constraint to consequence_reading when artifact_reading or principle_reading is the operative kernel reading, which would change the beneficiary/victim structure (artifact_reading burdens anyone who wants a modern-looking device regardless of effect; principle_reading burdens anyone needing grid-tied or leased infrastructure regardless of visiting effect).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consequence_reading_committer_structure, conceptual, 'Locating exactly where the three kernel readings diverge in their treatment of identical technology cases.').

omega_variable(
    visiting_density_causal_link,
    'Does restricting home telephones and similar technologies actually cause higher visiting density and better mutual-aid outcomes, or do stricter districts simply select for members already predisposed toward high-density community life (reverse causation / selection effect)?',
    'Longitudinal comparison of visiting frequency and mutual-aid participation in districts before and after specific rule changes (e.g., a district relaxing its phone-in-home rule), controlling for out-migration of members who disagreed with the stricter rule before the change.',
    'If the effect is mostly selection rather than causation, the consequence test''s claimed coordination function is weaker than authored, and the extractiveness score for payers whose costs are real but whose benefit-to-the-collective is overstated should be revised upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(visiting_density_causal_link, empirical, 'Whether restriction causes preserved visiting or merely correlates with a self-selected population.').

omega_variable(
    exit_cost_asymmetry,
    'Formal exit (declining baptism, leaving during Rumspringa) exists for all members, but is it equally available in practice to technologically curious youth versus members with dispersed family who are already older and more embedded?',
    'Interview or survey data on actual departure patterns by age and family structure, comparing stated reasons for leaving against stated reasons for staying despite disagreement with specific rulings.',
    'If exit is systematically harder for older, more embedded members with dispersed family, their exit_options classification (currently ''trapped'') is confirmed structurally rather than merely descriptively, strengthening the case that their cost bearing is a genuine asymmetry rather than a voluntarily accepted tradeoff.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exit_cost_asymmetry, empirical, 'Whether formal exit availability is uniform across payer subgroups or varies systematically by embeddedness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__consequence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__consequence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(gela_tr_t20, gelassenheit_separation__consequence_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__consequence_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement(gela_tr_t60, gelassenheit_separation__consequence_reading, theater_ratio, 60, 0.09).
narrative_ontology:measurement(gela_tr_t80, gelassenheit_separation__consequence_reading, theater_ratio, 80, 0.11).
narrative_ontology:measurement(gela_tr_t100, gelassenheit_separation__consequence_reading, theater_ratio, 100, 0.12).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__consequence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(gela_be_t20, gelassenheit_separation__consequence_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__consequence_reading, base_extractiveness, 40, 0.14).
narrative_ontology:measurement(gela_be_t60, gelassenheit_separation__consequence_reading, base_extractiveness, 60, 0.15).
narrative_ontology:measurement(gela_be_t80, gelassenheit_separation__consequence_reading, base_extractiveness, 80, 0.17).
narrative_ontology:measurement(gela_be_t100, gelassenheit_separation__consequence_reading, base_extractiveness, 100, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__consequence_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(gela_su_t20, gelassenheit_separation__consequence_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(gela_su_t40, gelassenheit_separation__consequence_reading, suppression_requirement, 40, 0.39).
narrative_ontology:measurement(gela_su_t60, gelassenheit_separation__consequence_reading, suppression_requirement, 60, 0.4).
narrative_ontology:measurement(gela_su_t80, gelassenheit_separation__consequence_reading, suppression_requirement, 80, 0.41).
narrative_ontology:measurement(gela_su_t100, gelassenheit_separation__consequence_reading, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__consequence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__consequence_reading, 0.1).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__principle_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__artifact_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the gelassenheit_separation kernel (network family). consequence_reading (this story) authors low extraction and a genuine, empirically-checkable coordination function centered on visiting/mutual-aid preservation. artifact_reading authors a different beneficiary/victim structure keyed to visible distinction rather than measured effect. principle_reading authors extraction keyed to functional/structural entanglement with worldly systems rather than to visiting effect. The same telephone-in-barn case can be ruled differently under each reading; per the ε-invariance principle these are three separate constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
