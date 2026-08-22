% ============================================================================
% CONSTRAINT STORY: second_amendment_text__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__individual_right_reading, []).

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
 *   constraint_id: second_amendment_text__individual_right_reading
 *   human_readable: Second Amendment — Individual Right Reading (Operative-Clause Liberty Floor)
 *   domain: constitutional law/political theory/firearms policy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the second_amendment_text kernel:
 *   the individual_right_reading, under which the operative clause guarantees
 *   a personal right independent of militia service and personal self-defense
 *   is the core protected activity. The ε referent is the standing
 *   arrangement under contest — the post-Heller/Bruen doctrinal landscape as
 *   it actually operates — assessed by this reading's own lights, NOT the
 *   fully deregulated arrangement the reading's strongest adherents endorse.
 *   The sibling readings (collective_security_reading,
 *   originalist_civic_virtue_reading) are separate constraints in separate
 *   files with their own ε values, beneficiary/victim structures, and
 *   classifications; nothing about them is averaged into this story. KEY
 *   AGENTS (by structural relationship): - individual_gun_owners: Primary
 *   beneficiary (organized/identity_locked) — holds the protected liberty -
 *   firearms_manufacturers_retailers: Material beneficiary
 *   (powerful/arbitrage) — collects the commercial rents of expansion -
 *   gun_rights_advocacy_organizations: Litigating beneficiary
 *   (organized/identity_locked) — sustained by the arrangement's defense -
 *   prohibited_persons_felony_records: Primary target (powerless/trapped) —
 *   bears the disarmed-line cost - domestic_abuse_misdemeanant_convictees:
 *   Target (powerless/trapped) — categorical exclusion as settled doctrine -
 *   urban_communities_violence_externalities: Externality bearer
 *   (moderate/constrained) - state_local_governments: Dual-positioned
 *   payer/administrator (institutional/constrained) - federal_judiciary:
 *   Agenda setter (institutional/constrained) — authors the perimeter -
 *   gun_violence_public_health_researchers: Excluded voice (moderate/mobile)
 *   - constitutional_law_scholars: Analytical observer (moderate/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, 0.55).
domain_priors:suppression_score(second_amendment_text__individual_right_reading, 0.56).
domain_priors:theater_ratio(second_amendment_text__individual_right_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__individual_right_reading, "Second Amendment — Individual Right Reading (Operative-Clause Liberty Floor)").
narrative_ontology:topic_domain(second_amendment_text__individual_right_reading, "constitutional law/political theory/firearms policy").

domain_priors:requires_active_enforcement(second_amendment_text__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__individual_right_reading, '1952a20e-3b8f-4aee-8d06-f9ffc0036863').
narrative_ontology:cs_kernel_codification('1952a20e-3b8f-4aee-8d06-f9ffc0036863', fixed_text).
narrative_ontology:cs_authority_grounding('1952a20e-3b8f-4aee-8d06-f9ffc0036863', lineage).
narrative_ontology:cs_interpretation_layer_present('1952a20e-3b8f-4aee-8d06-f9ffc0036863').
narrative_ontology:cs_reading_relation('1952a20e-3b8f-4aee-8d06-f9ffc0036863', second_amendment_text__collective_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('1952a20e-3b8f-4aee-8d06-f9ffc0036863', second_amendment_text__originalist_civic_virtue_reading, influences).
narrative_ontology:cs_axiom('1952a20e-3b8f-4aee-8d06-f9ffc0036863', foundational, right_independent_of_militia_service).
narrative_ontology:cs_axiom_status(right_independent_of_militia_service, holdable).
narrative_ontology:cs_axiom_grounding('1952a20e-3b8f-4aee-8d06-f9ffc0036863', right_independent_of_militia_service, deontological).
narrative_ontology:cs_axiom('1952a20e-3b8f-4aee-8d06-f9ffc0036863', foundational, personal_self_defense_core_activity).
narrative_ontology:cs_axiom_status(personal_self_defense_core_activity, holdable).
narrative_ontology:cs_axiom_grounding('1952a20e-3b8f-4aee-8d06-f9ffc0036863', personal_self_defense_core_activity, deontological).
narrative_ontology:cs_axiom('1952a20e-3b8f-4aee-8d06-f9ffc0036863', secondary, militia_preamble_prefatory_not_operative).
narrative_ontology:cs_axiom_status(militia_preamble_prefatory_not_operative, holdable).
narrative_ontology:cs_axiom_grounding('1952a20e-3b8f-4aee-8d06-f9ffc0036863', militia_preamble_prefatory_not_operative, conventional).
narrative_ontology:cs_reference_frame('1952a20e-3b8f-4aee-8d06-f9ffc0036863', codified_preexisting_individual_right).
narrative_ontology:cs_drift_state('1952a20e-3b8f-4aee-8d06-f9ffc0036863', contemporary_bruen_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('1952a20e-3b8f-4aee-8d06-f9ffc0036863', '').
narrative_ontology:cs_kernel_id(second_amendment_text__individual_right_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, firearms_manufacturers_retailers).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, prohibited_persons_felony_records).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, domestic_abuse_misdemeanant_convictees).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, urban_communities_violence_externalities).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, state_local_governments).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, heller_individual_right_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, text_history_tradition_adjudication).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, inherent_self_defense_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Roughly a third of American households keep firearms for self-defense, hunting, and sport. The doctrine shields their acquisition, possession, and (since Bruen) public carry from state-by-state variation, converting what was previously a regulated privilege in many jurisdictions into a protected liberty. Gun ownership is fused with political and social identity for a large subset; exit means divesting firearms and leaving gun culture, which carries real social cost.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, identity_locked, national).

% Sell into a legally protected demand curve. The liability shield statute insulates them from most tort exposure, and each doctrinal expansion (incorporation, carry rights) widens adjacent markets in permits, training, insurance, and accessories. They can shift product lines, channels, and marketing faster than any other seat; their exposure to the constraint's costs is minimal.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, firearms_manufacturers_retailers, beneficiary,
    powerful, generational, arbitrage, national).

% Litigate to expand and defend the right and mobilize members around its defense. Dues, relevance, and staffing scale with perceived threat to the right; their organizational identity is constituted by the constraint's maintenance. They collect no direct regulatory rent but are sustained by the arrangement's continuation.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, gun_rights_advocacy_organizations, beneficiary,
    organized, generational, identity_locked, national).

% Tens of millions of adults with felony records are categorically barred from firearm possession under prohibitions this reading affirms as presumptively lawful. The line between the protected and the disarmed runs through them; they bear the boundary-maintenance cost of the right without any constitutional recourse under it. Their status is effectively permanent, and as a class they have no coalition vehicle — fragmented by stigma, geography, and the political toxicity of advocating for their inclusion.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, prohibited_persons_felony_records, payer,
    powerless, biographical, trapped, national).

% Conviction of a misdemeanor crime of domestic violence triggers lifetime federal disarmament. Recent litigation affirming these prohibitions under the individual-right framework means this seat bears exclusion as the settled price of the doctrine's core. They hold no seat in the doctrine-making conversation and their disarmament is cited as evidence the framework is reasonable.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, domestic_abuse_misdemeanant_convictees, payer,
    powerless, biographical, trapped, national).

% Residents of cities where gun homicide concentrates. Their preferred regulatory tools (permit regimes, category bans, sensitive-place expansions) are repeatedly struck or chilled under the doctrine, while the armed prevalence the doctrine protects generates security costs, grief, and policing surges they absorb locally. Moving away is expensive; their policy preferences enter the process mainly as defendants' interests in litigation framed by the right.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, urban_communities_violence_externalities, payer,
    moderate, biographical, constrained, local).

% Enact and administer permit schemes, sensitive-place rules, and prohibited-category enforcement — the machinery the doctrine presupposes — while losing a substantial share of that same machinery to invalidation under the text-history-tradition test. They absorb the litigation costs of defending statutes that courts increasingly strike, and they cannot exit the constitutional framework they operate within.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, state_local_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__individual_right_reading, state_local_governments, agenda_setter).

% The Supreme Court and the circuits author the doctrine's content: the governing tests, the catalog of permissible prohibitions, the definition of sensitive places. Bound by precedent and appointment politics, they determine the right's perimeter case by case. They administer the constraint rather than collect from it, but no feature of the arrangement changes without them.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Produce the evidence base on costs, prevention, and policy effects. Federal funding for this work was long suppressed by appropriations riders, and their findings enter the process chiefly as contested exhibits in litigation. They would redesign the policy conversation around measured outcomes if seated; instead they stand outside the doctrine-making loop.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, gun_violence_public_health_researchers, excluded,
    moderate, generational, mobile, national).

% Parse the text, the ratification record, and the tradition evidence. Split across the three readings of the kernel, they supply the historical material courts select from and critique the selection afterward. They neither collect nor pay; their stake is interpretive.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, constitutional_law_scholars, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:fixing_cost_class(second_amendment_text__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform constitutional floor for individual decisions to keep and bear arms, coordinating expectations between citizens and fifty-plus regulatory jurisdictions about which regulations are permissible; preserves decentralized self-protection in a legal environment where police have no general duty to protect any particular individual.
% TRANSFER_FUNCTION: Moves regulatory authority from state and local governments to individual discretion; moves the costs of drawing the armed/disarmed line onto excluded classes (prohibited persons) and the costs of armed prevalence onto the communities where violence concentrates; moves litigation risk from manufacturers to governments and shooting victims.
% ABSENT_VOICES: Public-health researchers (defunded for a generation), survivors of gun violence, and prohibited persons themselves have no seat in doctrine formation — they appear as data points, defendants, or excluded classes rather than participants. Their objections surface only obliquely, in litigation framed by the right's own terms.
% DISAPPEARANCE_RATIONALE: If the individual-right guarantee vanished overnight, state regimes would immediately diverge — may-issue and prohibitionist frameworks would revive where repealed, carry norms would fragment regionally, the litigation economy built on the doctrine would collapse, and industry demand patterns would reshuffle. Nothing about the underlying society stays put; the arrangement is load-bearing for the current allocation of regulatory authority.
% FOUNDING_PROBLEM: Whether the amendment's operative clause protects individuals rather than states or militia institutions, and — following from that — shielding individual armament decisions, especially for self-defense, from majoritarian regulation that had come to treat the provision as a dead letter or a states'-rights placeholder.
% FOUNDING_PROBLEM_CORROBORATION: Federal courts in every circuit, including panels hostile to expansion, apply the individual right rather than denying it; state governments litigating restrictions concede the right's existence and contest only its perimeter; historical scholarship outside advocacy circles corroborates that founding-era arms possession was substantially tied to self-defense while disputing how much constitutional weight that fact should carry. No source outside the benefiting parties attests to the correctness of the current perimeter — corroboration covers the right's existence, not its extent.
narrative_ontology:disappearance_verdict(second_amendment_text__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__individual_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_text__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__individual_right_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.55 at interval end) because the arrangement's dominant operation is genuinely protective — tens of millions of net-beneficiary holders — while a real transfer layer runs beneath it: categorical disarmament of a large excluded class, externalized violence costs concentrated on urban communities, and regulatory authority stripped from governments. Suppression (0.56) reflects institutional rather than physical coercion: rival regulatory arrangements are struck, rival readings are marginalized doctrinally, and excluded classes cannot exit their status. Suppression is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness is scaled, by directionality and spatial scope, in the engine's computation. Theater (0.36) captures the growing performative share: tradition-hunting under the text-history-tradition method produces curated 1791 analogues of dubious decision relevance, and the surrounding symbolic politics (rallies, scorekeeping, anniversary litigation) is increasingly detached from the doctrine's functional operation. Resistance is high (0.7): the constraint meets continuous organized opposition from governments, public-health institutions, and advocacy movements, in both litigation and electoral registers. Accessibility collapse sits mid-range (0.5): alternative regulatory arrangements persist but are steadily foreclosed. The measurement series run on one shared time grid (2008, 2010, 2014, 2018, 2022, 2026) with every tracked metric authored at every point; trajectories are monotonic, not cyclical — each doctrinal expansion ratchets extraction and enforcement intensity upward without a compensating relaxation phase.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the divergence is the finding. From the individual_gun_owners seat the arrangement is near-pure coordination: a constitutional shield over a personal liberty, with the excluded class experienced as a reasonable boundary rather than a cost. From the prohibited_persons and domestic_abuse_convictee seats the same text operates as categorical exclusion enforced by the full weight of judicial review, with no exit and no coalition vehicle. From the state_local_governments seat it is an obligation machine that generates litigation they must fund and lose. From the federal_judiciary seat it is a neutral interpretive method. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: individual_gun_owners (identity_locked exit deepens their subsidy-side position), firearms_manufacturers_retailers (arbitrage-grade exit puts them nearest the full-beneficiary end — they capture upside and can route around downside), and gun_rights_advocacy_organizations (mission-fused collectors). Targets derive high directionality: prohibited_persons_felony_records and domestic_abuse_misdemeanant_convictees are trapped (no exit from status), amplifying effective extraction; urban_communities_violence_externalities are constrained (costly relocation) at local scope; state_local_governments are institutional payers whose enforcement role partially offsets their payer position. The federal_judiciary administers without collecting — its directionality sits near symmetric, weighted slightly toward the beneficiary side because the doctrine's expansion enlarges the judiciary's own domain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: individual self-protection needs persist, state regulatory variation persists, and the question the doctrine answers remains contested at the perimeter. Mandatrophy is therefore NOT declared — no sunset concept applies to a constitutional floor, and no atrophied-function signature is present. The tangled_rope classification does specific preventive work here: a rope-only reading would erase the identifiable disarmed victim set and the concentrated externality bearers; a snare reading would erase the genuine protective core that tens of millions of net beneficiaries experience daily. The hybrid is the structurally honest description, and the temporal series shows the extraction layer thickening over the interval — the direction of drift is toward the extractive pole, which the T17-class accumulation signal can register without forcing a premature reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the second_amendment_text kernel — the individual_right_reading. Would adopting a sibling reading change the structural classification wholesale?',
    'Comparative classification across the three sibling stories: if the collective_security_reading computes as rope (states coordinated, regulation as implementation) while this reading computes as tangled_rope, the divergence locates the contest in the beneficiary/victim structure rather than in any metric.',
    'Under the collective_security_reading the victim set largely vanishes (regulation becomes the coordination itself) and ε drops sharply; under the originalist_civic_virtue_reading the beneficiary set shifts to citizen-soldier capacity and the self-defense core loses its foundational status. The classification of THIS file is valid only for THIS parse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three readings of a contested kernel; siblings instantiate different constraints.').

omega_variable(
    exclusion_perimeter_trajectory,
    'Will the presumptively-lawful prohibitions (felon, domestic-abuser disarmament) hold as settled exceptions, or erode under the text-history-tradition method''s own logic?',
    'Track the litigation stream: appellate treatment of Range-type challenges, post-Rahimi lower-court behavior, and whether the tradition catalog for prohibited classes stabilizes or fragments.',
    'If the carve-outs erode, the victim set contracts dramatically and ε falls toward rope territory; if they harden into entrenched categorical exclusion, the extraction layer deepens and the arrangement drifts snare-ward. The current classification assumes partial stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_perimeter_trajectory, empirical, 'Stability of the disarmed-population boundary that generates much of the measured extraction.').

omega_variable(
    externality_attribution,
    'How much of the violence cost borne by urban communities is attributable to the right-protected prevalence the doctrine shields, versus confounders (poverty, policing practice, trafficking)?',
    'Quasi-experimental designs exploiting jurisdictional variation in permit regimes before and after Bruen, with synthetic controls; triangulated against the public-health literature the Dickey-era funding gap left thin.',
    'If attribution is low, urban_communities_violence_externalities drop out of the victim set and the classification relaxes toward rope; if high, the externality transfer is confirmed as a structural feature and extraction estimates rise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(externality_attribution, empirical, 'Causal share of community violence costs properly charged to the constraint.').

omega_variable(
    defensive_use_magnitude,
    'What is the true annual rate of defensive gun use — the coordination-side benefit the reading cites as core?',
    'Methodologically disciplined survey replication and incident-based estimation replacing the disputed order-of-magnitude spread in the existing literature.',
    'Estimates currently span roughly 60,000 to 2.5 million incidents annually. At the low end the coordination function thins and the extraction share dominates; at the high end the protective core justifies a lower net-ε reading than authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(defensive_use_magnitude, empirical, 'Magnitude of the benefit side of the coordination/extraction balance.').

omega_variable(
    identity_lock_durability,
    'How durable is the identity fusion binding individual_gun_owners and gun_rights_advocacy_organizations to the constraint?',
    'Longitudinal tracking of gun-ownership self-description against partisan identity: if armament decouples from political identity in cohort replacement, the lock loosens.',
    'Identity-locked exit amplifies both the beneficiaries'' persistence pressure and the intensity of resistance the constraint meets. If the frame breaks, beneficiary cohesion drops, resistance softens, and the arrangement''s stability becomes dependent on the judiciary alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_durability, conceptual, 'Durability of the identity-fusion mechanism sustaining the beneficiary coalition.').

omega_variable(
    authority_grounding_framing,
    'Is the authority structure grounding this reading''s legitimacy best framed as lineage (continuity with the founding text) or as extraction (institutional actors deriving authority from controlling the interpretation)?',
    'Examine where interpretive control actually concentrates: Article V formality and the fixed-text character push toward lineage; the selective tradition-hunting under the text-history-tradition method, where the interpreting court curates which 1791 analogues count, pushes toward extraction.',
    'Signals favoring the lineage choice: the kernel is a formally entrenched text amendable only by supermajority, and no interpreter claims power to rewrite it. If the extraction framing were adopted instead, the authority structure would classify as self-perpetuating interpretive capture, shifting the CS pattern and strengthening the case that the doctrine''s drift is managed rather than discovered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'CS-framing under-determination: lineage versus extraction as the authority ground.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__individual_right_reading, 2008, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_indiv_right_tr_t2008, second_amendment_text__individual_right_reading, theater_ratio, 2008, 0.2).
narrative_ontology:measurement_basis(sa_indiv_right_tr_t2008, observed).
narrative_ontology:measurement(sa_indiv_right_tr_t2010, second_amendment_text__individual_right_reading, theater_ratio, 2010, 0.23).
narrative_ontology:measurement_basis(sa_indiv_right_tr_t2010, observed).
narrative_ontology:measurement(sa_indiv_right_tr_t2014, second_amendment_text__individual_right_reading, theater_ratio, 2014, 0.26).
narrative_ontology:measurement_basis(sa_indiv_right_tr_t2014, observed).
narrative_ontology:measurement(sa_indiv_right_tr_t2018, second_amendment_text__individual_right_reading, theater_ratio, 2018, 0.29).
narrative_ontology:measurement_basis(sa_indiv_right_tr_t2018, observed).
narrative_ontology:measurement(sa_indiv_right_tr_t2022, second_amendment_text__individual_right_reading, theater_ratio, 2022, 0.33).
narrative_ontology:measurement_basis(sa_indiv_right_tr_t2022, observed).
narrative_ontology:measurement(sa_indiv_right_tr_t2026, second_amendment_text__individual_right_reading, theater_ratio, 2026, 0.36).
narrative_ontology:measurement_basis(sa_indiv_right_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(sa_indiv_right_be_t2008, second_amendment_text__individual_right_reading, base_extractiveness, 2008, 0.42).
narrative_ontology:measurement_basis(sa_indiv_right_be_t2008, observed).
narrative_ontology:measurement(sa_indiv_right_be_t2010, second_amendment_text__individual_right_reading, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement_basis(sa_indiv_right_be_t2010, observed).
narrative_ontology:measurement(sa_indiv_right_be_t2014, second_amendment_text__individual_right_reading, base_extractiveness, 2014, 0.47).
narrative_ontology:measurement_basis(sa_indiv_right_be_t2014, observed).
narrative_ontology:measurement(sa_indiv_right_be_t2018, second_amendment_text__individual_right_reading, base_extractiveness, 2018, 0.49).
narrative_ontology:measurement_basis(sa_indiv_right_be_t2018, observed).
narrative_ontology:measurement(sa_indiv_right_be_t2022, second_amendment_text__individual_right_reading, base_extractiveness, 2022, 0.53).
narrative_ontology:measurement_basis(sa_indiv_right_be_t2022, observed).
narrative_ontology:measurement(sa_indiv_right_be_t2026, second_amendment_text__individual_right_reading, base_extractiveness, 2026, 0.55).
narrative_ontology:measurement_basis(sa_indiv_right_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(sa_indiv_right_su_t2008, second_amendment_text__individual_right_reading, suppression_requirement, 2008, 0.38).
narrative_ontology:measurement_basis(sa_indiv_right_su_t2008, observed).
narrative_ontology:measurement(sa_indiv_right_su_t2010, second_amendment_text__individual_right_reading, suppression_requirement, 2010, 0.41).
narrative_ontology:measurement_basis(sa_indiv_right_su_t2010, observed).
narrative_ontology:measurement(sa_indiv_right_su_t2014, second_amendment_text__individual_right_reading, suppression_requirement, 2014, 0.44).
narrative_ontology:measurement_basis(sa_indiv_right_su_t2014, observed).
narrative_ontology:measurement(sa_indiv_right_su_t2018, second_amendment_text__individual_right_reading, suppression_requirement, 2018, 0.47).
narrative_ontology:measurement_basis(sa_indiv_right_su_t2018, observed).
narrative_ontology:measurement(sa_indiv_right_su_t2022, second_amendment_text__individual_right_reading, suppression_requirement, 2022, 0.52).
narrative_ontology:measurement_basis(sa_indiv_right_su_t2022, observed).
narrative_ontology:measurement(sa_indiv_right_su_t2026, second_amendment_text__individual_right_reading, suppression_requirement, 2026, 0.56).
narrative_ontology:measurement_basis(sa_indiv_right_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__originalist_civic_virtue_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, plcaa_firearm_liability_shield).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, dickey_amendment_research_funding_bar).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Second Amendment' decomposes, per the ε-invariance principle, into three structurally distinct readings of one fixed-text kernel. This file instantiates the individual_right_reading (beneficiary: individual holders; victim set: categorically disarmed classes and externality-bearing communities; ε ≈ 0.55). The collective_security_reading (separate file) yields a different beneficiary set (states/militia institutions), folds regulation into the coordination function, and computes a far lower ε. The originalist_civic_virtue_reading (separate file) shifts the protected good to citizen-soldier capacity. The upstream/downstream structure runs from this reading outward: its doctrinal victories (Heller, McDonald, Bruen) supply the interpretive method and judicial personnel that the civic-virtue reading draws on (influences edge), while the collective-security reading persists as a live rival position held by dissenting factions (coexistence edge). Each member links to the others; none is evaluable in isolation because each ε is defined against the same standing text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
