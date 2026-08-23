% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__expansive_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__expansive_construction_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__expansive_construction_reading
 *   human_readable: Expansive Construction Reading: Artificial Features Generate De Facto Territorial Waters via Effective Occupation
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This story instantiates the expansive_construction_reading of the
 *   unclos_maritime_sovereignty kernel: the position that artificial island
 *   construction on submerged features and low-tide elevations generates de
 *   facto territorial waters through effective occupation and administrative
 *   control. The standing arrangement under contest — the epsilon referent
 *   per the kernel-reading rule — is the enforced de facto regime around
 *   constructed features: territorial-sea rings and claimed broader zones,
 *   administered and defended by the constructing state, imposed on
 *   neighboring claimants' overlapping entitlements and on navigational
 *   freedoms the kernel itself protects. The 2016 annex VII arbitral award,
 *   the strict_geographic_reading's instrument, held that artificial
 *   construction does not alter legal status and that low-tide elevations
 *   generate no entitlements; the constructing state rejects the award's
 *   validity and continues the practice, which is why the arrangement
 *   persists by enforcement rather than by settled law. Per the
 *   epsilon-invariance principle this file authors ONE reading with ONE
 *   stable epsilon; the strict and hybrid siblings are separate stories
 *   linked through network.affects_constraints. Reading-indexed note: because
 *   this reading legitimizes the arrangement, it authors the referent's
 *   extraction at a moderate value (0.4) — the strict_geographic_reading
 *   story will assess the SAME arrangement at substantially higher epsilon;
 *   that cross-reading divergence over a shared referent is precisely the
 *   measurement the kernel decomposition exists to take. The claim and the
 *   metrics are independent authored facts: claimed_type tangled_rope
 *   reflects my structural judgment that the arrangement couples a thin
 *   genuine coordination layer (real search-and-rescue, weather, and
 *   navigation services, plus a determinate crystallization rule where the
 *   text is indeterminate) to asymmetric extraction enforced by coercion; the
 *   engine computes each seat's type from the structural data, and where a
 *   computed seat type diverges from this claim, that divergence is the
 *   datum.
 *
 * KEY AGENTS:
 *   - island_constructing_states: agenda-setter and primary beneficiary (institutional / arbitrage) — builds, administers, and enforces; collects the territorial waters, resource jurisdiction, and strategic depth
 *   - neighboring_claimant_states: primary payers (organized / constrained) — hold overlapping claims the arrangement encroaches; contest through protest, arbitration, and rival construction
 *   - fon_naval_operators: payers with mobile exit (institutional / mobile) — global navies whose passages the arrangement's enforcement challenges; they contest by sailing
 *   - traditional_regional_fishers: most exposed payers (powerless / trapped) — lose traditional grounds to enclosure and licensing
 *   - arbitral_dispute_institutions: excluded seat — issued the award the arrangement refuses to give effect to
 *   - asean_collective_forum: excluded seat — consensus rules muffle collective objection
 *   - maritime_law_scholarship: analytical observer — tracks the reading against the kernel text, the award, and state practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, 0.4).
domain_priors:suppression_score(unclos_maritime_sovereignty__expansive_construction_reading, 0.7).
domain_priors:theater_ratio(unclos_maritime_sovereignty__expansive_construction_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__expansive_construction_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__expansive_construction_reading, "Expansive Construction Reading: Artificial Features Generate De Facto Territorial Waters via Effective Occupation").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__expansive_construction_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__expansive_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__expansive_construction_reading, '0ad70235-73d3-4407-8b2b-d9017cd3120d').
narrative_ontology:cs_kernel_codification('0ad70235-73d3-4407-8b2b-d9017cd3120d', formalized).
narrative_ontology:cs_authority_grounding('0ad70235-73d3-4407-8b2b-d9017cd3120d', extraction).
narrative_ontology:cs_interpretation_layer_present('0ad70235-73d3-4407-8b2b-d9017cd3120d').
narrative_ontology:cs_reading_relation('0ad70235-73d3-4407-8b2b-d9017cd3120d', unclos_maritime_sovereignty__strict_geographic_reading, forecloses).
narrative_ontology:cs_reading_relation('0ad70235-73d3-4407-8b2b-d9017cd3120d', unclos_maritime_sovereignty__hybrid_effective_control_reading, influences).
narrative_ontology:cs_axiom('0ad70235-73d3-4407-8b2b-d9017cd3120d', foundational, effective_occupation_generates_maritime_title).
narrative_ontology:cs_axiom_status(effective_occupation_generates_maritime_title, holdable).
narrative_ontology:cs_axiom_grounding('0ad70235-73d3-4407-8b2b-d9017cd3120d', effective_occupation_generates_maritime_title, empirically_contingent).
narrative_ontology:cs_axiom('0ad70235-73d3-4407-8b2b-d9017cd3120d', secondary, administrative_control_evidences_sovereign_intent).
narrative_ontology:cs_axiom_status(administrative_control_evidences_sovereign_intent, holdable).
narrative_ontology:cs_axiom_grounding('0ad70235-73d3-4407-8b2b-d9017cd3120d', administrative_control_evidences_sovereign_intent, conventional).
narrative_ontology:cs_reference_frame('0ad70235-73d3-4407-8b2b-d9017cd3120d', effective_occupation_title_regime).
narrative_ontology:cs_drift_state('0ad70235-73d3-4407-8b2b-d9017cd3120d', post_arbitral_award_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0ad70235-73d3-4407-8b2b-d9017cd3120d', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, fon_naval_operators).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, traditional_regional_fishers).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__expansive_construction_reading, effective_occupation_doctrine).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__expansive_construction_reading, administrative_control_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Builds and equips artificial islands on reefs and low-tide elevations, stations garrisons and civil administrators, and declares administrative districts over the surrounding waters. Enforces the claimed zones with coast guard patrols, maritime militia, radar coverage, and missile emplacements; expels foreign fishing vessels and challenges foreign warship passages. Funds construction and enforcement from national budgets and collects the enclosed waters' fisheries access, resource jurisdiction, and strategic depth. Exit would mean abandoning sunk infrastructure and a nationalist commitment, but the seat retains flexibility to pause, reframe, or trade individual claims.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states, beneficiary).

% Hold overlapping claims to the same features and waters under their own readings of the treaty text. Lose access to fishing grounds and resource areas inside the enforced zones; contest through diplomatic protest, an annex VII arbitration they won but cannot execute, coast guard standoffs, and their own smaller-scale construction. Cannot leave the region, and their claim behavior is set inside the arrangement's shadow.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states, payer,
    organized, generational, constrained, regional).

% Global navies whose vessels transit and operate in the enclosed waters under freedom-of-navigation policy. The arrangement's enforcement challenges their passages and requires repeated assertion operations to keep transit rights legible. They can redirect global posture elsewhere at will, which keeps their contest discretionary rather than existential.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, fon_naval_operators, payer,
    institutional, generational, mobile, global).

% Work grounds their communities have fished for generations, adjacent to home ports. Inside the enforced zones they are expelled, licensed, or detained; catch volumes and grounds have contracted. Relocating means longer, costlier, more dangerous trips or leaving the trade; for many the fishing livelihood is inherited and the grounds are the family's only capital.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, traditional_regional_fishers, payer,
    powerless, immediate, trapped, local).

% Issued a 2016 annex VII award holding that artificial construction does not alter legal status and that low-tide elevations generate no maritime entitlements. The constructing state rejected the award's validity and refuses compliance; the institution has no execution arm, and its output is excluded from the arrangement's operative effect while remaining on the record.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, arbitral_dispute_institutions, excluded,
    institutional, generational, analytical, global).

% The regional diplomatic body whose membership includes both the constructing state and several claimants. Its consensus rule lets a single member block joint statements, so collective objection to the arrangement is structurally muffled and individual members negotiate bilaterally from weakness.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, asean_collective_forum, excluded,
    organized, generational, constrained, regional).

% Academic and institutional commentators who track the reading against the treaty text, the award, and state practice. They publish the strict-reading consensus, document the construction's environmental destruction of reef systems, and hold no enforcement role; their seat is analytical.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, maritime_law_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__expansive_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides maritime governance services — search-and-rescue, meteorological reporting, navigation aids, fisheries administration — from constructed installations in a region where such services were thin, and supplies a determinate crystallization rule (effective occupation) where the kernel text is indeterminate on artificial features.
% TRANSFER_FUNCTION: Moves maritime space and jurisdiction — territorial-sea rings and claimed broader zones around constructed features — from neighboring claimant states and the high-seas commons to the island-constructing state; moves traditional fishing grounds from regional fishers to the constructing state's licensed fleet; imposes recurring contestation costs on freedom-of-navigation operators who must physically assert passage.
% ABSENT_VOICES: Traditional fishers bear the enclosure but sit outside the diplomatic and legal fora where the arrangement is contested. The strict-reading position speaks through the 2016 arbitral award but is excluded from operative effect by the constructing state's rejection. ASEAN's consensus rule muffles collective objection. The reef ecosystems destroyed by dredging have no seat in any forum.
% DISAPPEARANCE_RATIONALE: If the construction-generated waters and their enforcement vanished overnight, the constructing state would lose forward positions and claimed zones; neighboring claimants would resume operations in the enclosed areas; freedom-of-navigation assertion operations would stand down; fishing access would revert; and the region's claim map would reorganize around natural-feature entitlements.
% FOUNDING_PROBLEM: Securing exposed forward positions: the initial installations solved shelter, provisioning, and situational awareness for detachments holding remote contested features; doctrinally, the reading answered how a state normalizes holdings the treaty text does not clearly entitle it to.
% FOUNDING_PROBLEM_CORROBORATION: The constructing state attests the security problem remains live, citing exposed detachments and contested approaches. Outside the benefiting parties: the 2016 annex VII award and strict-reading scholarship attest that the arrangement's current operation — territorial-sea generation from submerged features — exceeds any security need the founding problem describes; freedom-of-navigation operators' public operational statements attest that the arrangement now functions as a regional-order claim rather than position security.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__expansive_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__expansive_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__expansive_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__expansive_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__expansive_construction_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).
:- end_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is 0.4 and reading-indexed: the expansive reading prices the arrangement's imposition on neighbors and navigation as the lawful consequence of superior effective control — costs it acknowledges (fisher expulsions, freedom-of-navigation challenges, reef destruction) but does not count as extraction. A lower value would require denying costs the arrangement's own enforcement makes undeniable; a higher value would be the strict reading's assessment, not this one's. Suppression is 0.7 as a raw structural property, unscaled by power or scope (only extractiveness is engine-scaled): the arrangement's persistence depends on coast guard expansion, maritime militia, radar and missile infrastructure, expulsion of fishing vessels, and physical challenge to warship passages. Theater_ratio is 0.45 and rising because the effective-occupation doctrine structurally rewards performative administration — occupation-evidence is load-bearing under this reading, so the theater is not decay but doctrine. Accessibility_collapse is 0.55: alternatives (the award, freedom-of-navigation assertion, rival construction) persist but at rising cost. Resistance is 0.65: sustained litigation, assertion operations, diplomatic protest, and competing building. The measurement series run on ONE shared time grid (T=0 approximates 1995, the first permanent structures on a submerged feature; T=30 approximates 2025, the militarized artificial archipelago), every tracked metric authored at every point. Suppression_requirement is authored because enforcement-capacity change IS this story's tracked dynamic: the enforcement machinery hardened monotonically across the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the constructing state's seat the arrangement is occupation and governance — entitlement crystallizing through use, services provided, order administered. From the neighboring claimant seats the same structure is enclosure of their own entitlements. From the freedom-of-navigation seats it is a navigation-freedom problem to be kept legible by repeated assertion. From the fisher seat it is dispossession of inherited grounds. The engine computes these per-seat classifications from power, exit, and role; the expected divergence — a rope-like read at the agenda-setter seat versus snare-like reads at the trapped payer seats — is the measurement, not a defect to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   island_constructing_states sit near the beneficiary end: declared beneficiary, arbitrage-grade exit (they can pause, reframe, or trade claims without losing the core position). neighboring_claimant_states are declared victims with constrained exit — high directionality toward the target end. traditional_regional_fishers are victims with trapped exit — the highest effective extraction of any seat, since the enclosure lands directly on their livelihood with no substitute grounds. fon_naval_operators are declared victims but with mobile exit: their effective extraction sits well below the trapped fishers' despite similar nominal targeting, because they can decline the theater entirely — which is exactly what distinguishes them from the regionally locked seats and why no directionality override is needed for them.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two mislabels. Reading the arrangement as pure coordination fails because the services are thin and separable — search-and-rescue and weather reporting do not require the territorial-waters claim and can be provided from any installation. Reading it as pure snare fails because the crystallization rule and the services are not pure cover: the kernel text genuinely is indeterminate on artificial features, and the reading resolves that indeterminacy, which has coordinating value among constructing states even if it creates a race-to-build dynamic regionally. The R5 genealogy shows the founding problem — securing exposed detachments — is contested rather than dead: the constructing state still faces real exposure, so this is not a zombie mandate; but the arrangement has outgrown that problem into regional-order restructuring, which is why founding_problem_status is contested rather than live. The mismatch consumer reads status x disappearance_verdict: contested x world_rearranges carries no dead-mandate flag, and the honest reading is a mandate that has expanded beyond its founding problem while remaining partly live. Coalition potential for the powerless seats exists on paper (a unified claimant coalition) but is structurally muffled by the ASEAN consensus rule, which is why the fisher seat's powerlessness is not offset by class coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Does construction on submerged features generate any maritime entitlement at all, or does the strict reading''s natural-formation prerequisite exhaust the kernel''s entitlement categories?',
    'Accumulated state practice, judicial reception in other fora, and diplomatic recognition patterns over the coming decades.',
    'If the strict reading prevails, the arrangement''s claimed waters dissolve, the victim set empties (the waters revert to their prior holders), and this story''s classification collapses from an operative constraint toward a failed claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the unclos_maritime_sovereignty kernel governs artificial-feature entitlements.').

omega_variable(
    hybrid_reading_delta,
    'What would the hybrid sibling change structurally — artificial features generating only 500m safety zones, maturing into territorial claims only through prolonged unchallenged control?',
    'Comparative classification: run the same structural data under the hybrid reading''s scope and maturation conditions.',
    'Under the hybrid reading the victim set narrows to states that fail to challenge, extraction drops toward coordination-cost levels, and the arrangement''s classification shifts toward rope; the maturation clause also converts this reading''s practice into the hybrid''s evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hybrid_reading_delta, conceptual, 'Structural delta if the hybrid_effective_control_reading governed instead.').

omega_variable(
    defacto_dejure_status,
    'The reading''s own ''de facto'' qualifier — is the arrangement a legal constraint awaiting ratification, or a power fact that legal form would merely record?',
    'Track whether third states begin treating the waters as settled: charting practice, commercial licensing and insurance behavior, diplomatic language shifts.',
    'If merely a power fact, the constraint''s persistence depends entirely on enforcement capacity and snare-flavored dynamics dominate; if legal form is consolidating, enforcement dependency declines and the arrangement hardens into accepted entitlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defacto_dejure_status, conceptual, 'Whether the de facto waters are a constraint in formation or control awaiting legal record.').

omega_variable(
    occupation_maturation_window,
    'Does prolonged control mature the de facto waters into accepted entitlements, or does sustained contestation keep them provisional indefinitely?',
    'Challenge frequency, recognition patterns, and commercial behavior (shipping lanes, insurance pricing) tracked over the next two decades.',
    'Maturation would retroactively legitimize the extraction and drive this reading''s epsilon down; indefinite contest keeps the arrangement enforcement-dependent and the extraction reading live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupation_maturation_window, empirical, 'Whether the de facto regime consolidates or remains perpetually provisional under challenge.').

omega_variable(
    occupation_theater_share,
    'How much of the ''administrative control'' activity is occupation-evidence performance rather than governance function — the effective-occupation doctrine structurally rewards performative administration?',
    'Audit the installations'' service output (search-and-rescue cases handled, weather reports issued, fishery inspections conducted) against administrative-ritual activity (ceremonies, naming announcements, sovereignty tours, district-formation events).',
    'A high performance share would push theater_ratio above 0.5 and signal Goodhart drift of the administrative layer — administration performed to evidence occupation rather than to govern waters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupation_theater_share, empirical, 'Share of administrative activity that is occupation-evidence theater rather than governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__expansive_construction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(uncl_tr_t0, observed).
narrative_ontology:measurement(uncl_tr_t6, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement_basis(uncl_tr_t6, observed).
narrative_ontology:measurement(uncl_tr_t12, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement_basis(uncl_tr_t12, observed).
narrative_ontology:measurement(uncl_tr_t18, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 18, 0.35).
narrative_ontology:measurement_basis(uncl_tr_t18, observed).
narrative_ontology:measurement(uncl_tr_t24, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(uncl_tr_t24, observed).
narrative_ontology:measurement(uncl_tr_t30, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement_basis(uncl_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(uncl_be_t0, observed).
narrative_ontology:measurement(uncl_be_t6, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 6, 0.28).
narrative_ontology:measurement_basis(uncl_be_t6, observed).
narrative_ontology:measurement(uncl_be_t12, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 12, 0.31).
narrative_ontology:measurement_basis(uncl_be_t12, observed).
narrative_ontology:measurement(uncl_be_t18, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 18, 0.34).
narrative_ontology:measurement_basis(uncl_be_t18, observed).
narrative_ontology:measurement(uncl_be_t24, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement_basis(uncl_be_t24, observed).
narrative_ontology:measurement(uncl_be_t30, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement_basis(uncl_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(uncl_su_t0, observed).
narrative_ontology:measurement(uncl_su_t6, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 6, 0.46).
narrative_ontology:measurement_basis(uncl_su_t6, observed).
narrative_ontology:measurement(uncl_su_t12, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement_basis(uncl_su_t12, observed).
narrative_ontology:measurement(uncl_su_t18, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement_basis(uncl_su_t18, observed).
narrative_ontology:measurement(uncl_su_t24, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 24, 0.64).
narrative_ontology:measurement_basis(uncl_su_t24, observed).
narrative_ontology:measurement(uncl_su_t30, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(uncl_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__expansive_construction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (feature-generated maritime entitlements under UNCLOS), three readings with different epsilon values, victim sets, and classifications. The expansive reading authors the standing arrangement at moderate reading-indexed extraction (~0.4) because it legitimizes occupation; the strict_geographic_reading assesses the same arrangement as pure enclosure (substantially higher epsilon); the hybrid_effective_control_reading prices it between, keyed to challenge and maturation. The strict reading is the kernel text's most direct instantiation and the arbitral award's basis (upstream); this reading's occupation practice manufactures the prolonged-control facts the hybrid's maturation clause consumes (downstream pressure). Linked via affects_constraints in all three family files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
