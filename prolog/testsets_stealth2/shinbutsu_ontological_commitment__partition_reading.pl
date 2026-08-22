% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__partition_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__partition_reading
 *   human_readable: Shinto-Buddhist Domain Partition (Life-Cycle vs Afterlife)
 *   domain: religious/historical/ontological
 *
 * SUMMARY:
 *   The arrangement this story models is the standing division of ritual
 *   labor between Japan's two institutional traditions: shrines conduct the
 *   life-affirming calendar — birth presentations, weddings, festivals, New
 *   Year observances — while temples conduct death rites — funerals,
 *   memorials, grave-keeping — and laypeople move between them without
 *   conversion or any ruling on how kami and buddhas relate. The division
 *   crystallized under the Edo temple-registration system, was imposed
 *   coercively by the Meiji separation edicts of 1868, survived
 *   disestablishment in 1947 as pure custom, and now erodes through secular
 *   alternatives. This file instantiates the partition reading of the
 *   shinbutsu_ontological_commitment kernel; the syncretic and incoherence
 *   readings are separate stories. Claim and metrics are authored
 *   independently: the tangled_rope claim reflects the structure I take to be
 *   true (genuine coordination plus asymmetric death-domain transfer under
 *   active administration), while the metric values describe the
 *   arrangement's observed operation.
 *
 * KEY AGENTS:
 *   - - buddhist_funeral_establishment: Agenda-setter and primary receipt seat (organized/constrained) — administers the death domain, collects the recurring fees
 *   - - shinto_shrine_network: Protected beneficiary (organized/constrained) — holds the life-cycle and calendrical domain, bears no death-rite obligation
 *   - - danka_households: Primary payer (moderate/constrained) — funds both halves across generations; exit thinning
 *   - - secular_funeral_providers: Excluded alternative (moderate/mobile) — outside the customary conversation; the main erosion channel
 *   - - scholars_of_japanese_religion: Analytical observer — hosts the contest between the three readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__partition_reading, 0.46).
domain_priors:suppression_score(shinbutsu_ontological_commitment__partition_reading, 0.26).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__partition_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0.26).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__partition_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__partition_reading, "Shinto-Buddhist Domain Partition (Life-Cycle vs Afterlife)").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__partition_reading, "religious/historical/ontological").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__partition_reading, 'c7b08dc9-fd57-4c76-8cc6-26742e786dd8').
narrative_ontology:cs_kernel_codification('c7b08dc9-fd57-4c76-8cc6-26742e786dd8', distributed).
narrative_ontology:cs_authority_grounding('c7b08dc9-fd57-4c76-8cc6-26742e786dd8', practice).
narrative_ontology:cs_reading_relation('c7b08dc9-fd57-4c76-8cc6-26742e786dd8', shinbutsu_ontological_commitment__syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7b08dc9-fd57-4c76-8cc6-26742e786dd8', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('c7b08dc9-fd57-4c76-8cc6-26742e786dd8', foundational, jurisdictional_separation_suffices).
narrative_ontology:cs_axiom_status(jurisdictional_separation_suffices, holdable).
narrative_ontology:cs_axiom_grounding('c7b08dc9-fd57-4c76-8cc6-26742e786dd8', jurisdictional_separation_suffices, empirically_contingent).
narrative_ontology:cs_axiom('c7b08dc9-fd57-4c76-8cc6-26742e786dd8', foundational, dual_affiliation_needs_no_adjudication).
narrative_ontology:cs_axiom_status(dual_affiliation_needs_no_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('c7b08dc9-fd57-4c76-8cc6-26742e786dd8', dual_affiliation_needs_no_adjudication, conventional).
narrative_ontology:cs_reference_frame('c7b08dc9-fd57-4c76-8cc6-26742e786dd8', complementary_domain_settlement).
narrative_ontology:cs_drift_state('c7b08dc9-fd57-4c76-8cc6-26742e786dd8', contemporary_mass_secularization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c7b08dc9-fd57-4c76-8cc6-26742e786dd8', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, buddhist_funeral_establishment).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, shinto_shrine_network).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__partition_reading, danka_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, danka_households).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__partition_reading, complementary_domain_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__partition_reading, lay_dual_affiliation_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conducts funerals, memorial services, and grave-keeping for registered member households; maintains parish rolls inherited from the temple-registration era; collects recurring payments for services and grave sites; controls most cemetery plots attached to temple grounds. Reports membership decline openly as its central financial concern. Leaving the funeral economy would require surrendering its principal income and its cemetery holdings.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, buddhist_funeral_establishment, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__partition_reading, buddhist_funeral_establishment, beneficiary).

% Conducts birth presentations, weddings, festival rites, and New Year observances; receives offerings, patronage, and municipal festival support; does not conduct funerals and refers death-adjacent requests to Buddhist clergy. Its ritual calendar and finances assume the death domain sits elsewhere; taking up funeral work would require doctrinal and facility changes it has not made.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, shinto_shrine_network, beneficiary,
    organized, generational, constrained, national).

% Register with a neighborhood temple at household level and pay across generations for funerals, periodic memorial services, and grave maintenance, while also visiting shrines for births, weddings, and festivals. Receive complete ritual coverage across the life course in exchange. Considering exit means relocating ancestral graves, negotiating with the temple, and absorbing family disagreement; newer options such as tree burial and secular services are lowering that wall.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, danka_households, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__partition_reading, danka_households, beneficiary).

% Offer non-religious funerals, family graves, ash scattering, and lifetime funeral plans outside temple jurisdiction. Legally free to operate and growing, but absent from the customary conversations in which families arrange death rites; their expansion is the main channel through which households leave the temple relationship.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, secular_funeral_providers, excluded,
    moderate, biographical, mobile, national).

% Document the arrangement's history from the temple-registration system through the 1868 separation edicts to the postwar settlement, and host the disagreement over what commitment held it together. Neither collects from the arrangement nor bears its costs; several of its leading analysts argue the arrangement is best described by one of the rival readings rather than this one.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, scholars_of_japanese_religion, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__partition_reading, buddhist_funeral_establishment).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates ritual jurisdiction between two institutional traditions so that one population obtains full life-course coverage — birth, marriage, festival, death, memorial — without either tradition having to defeat the other and without any authority ruling on their metaphysical relation.
% TRANSFER_FUNCTION: Moves funeral, memorial, and grave-maintenance payments from registered households to Buddhist temples; moves offerings, wedding fees, and festival patronage toward shrines; and moves ritual legitimacy to whichever institution holds the relevant life domain.
% ABSENT_VOICES: Secular and Christian funeral providers sit outside the customary arrangement process; Shinto exclusivists and Buddhist reformers seeking single-tradition purity are marginal; and the deceased themselves — the stated object of the afterlife domain — have no voice in how their memorial care is organized or priced.
% DISAPPEARANCE_RATIONALE: If the partition vanished overnight, temples and shrines would confront each other's jurisdictions directly: shrines would face demands to handle death or justify refusal, temples would lose the captive funeral calendar or absorb life rites, households would need a single provider or explicit mixed practice, and the funeral market would reorganize around open competition. The rearrangement would be slow — graves and parish rolls are generational — but directional.
% FOUNDING_PROBLEM: How can two totalizing ritual systems share one population without forcing conversion or triggering zero-sum competition — specifically, who handles death, the domain neither tradition wished to yield but for which Buddhism alone possessed doctrinal apparatus (afterlife, karmic continuity, memorial obligation).
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by historians of Japanese religion working on the temple-registration system and the 1868 separation edicts, and by large-scale surveys of religious self-understanding showing widespread non-religious identity alongside continued ritual observance. Temple and shrine associations assert the founding problem's persistence, but no attestation from within the benefiting set is relied on here.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__partition_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__partition_reading_tests).
:- end_tests(shinbutsu_ontological_commitment__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   End-state extraction is 0.46: the partition delivers real coordination (full life-course coverage, peace between institutions) while channeling a persistent, hard-to-refuse transfer through the death domain, where grief makes households price-insensitive and graves make exit costly. Suppression is 0.26 — soft structural lock-in (cemetery control, parish rolls, family expectation) after the 1947 disestablishment removed the coercive layer. Theater is 0.60: a majority of participants self-identify as non-religious while performing both liturgical calendars, so a growing share of the arrangement's activity is customary performance rather than lived commitment. Accessibility_collapse is 0.45 — alternatives exist and are expanding (secular funerals, tree burial, family graves) but carry social friction, so understanding the arrangement does not collapse exits the way a natural limit would. Resistance is 0.40 — violent institutional resistance at the 1868 imposition was crushed, and modern consumer resistance (funeral-cost backlash, alternative-burial movements) is real but diffuse. The temporal series run on one shared seven-point grid with every tracked metric authored at every point. The suppression_requirement series declines monotonically because enforcement decay is the dynamic this story traces: the arrangement was founded at gunpoint in 1868, lost its enforcement machinery in 1947, and now runs on norms alone. Base extractiveness is hump-shaped rather than monotonic — an imposition spike, a postwar dip, a bubble-era rise as funeral spending inflated, and recent relief as secular alternatives opened.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the temple seat the arrangement is indispensable sacramental service unfairly caricatured as a billing machine; from the household seat it is obligatory invoicing attached to grief; from the shrine seat it is a fair specialization that keeps death-rite obligations off its books; from the excluded secular-provider seat it is a closed market defended by custom rather than quality. The engine derives these divergences from the structural data — the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The shrine network sits nearest the beneficiary end: it receives domain protection and pays nothing into the death side. The temple establishment is beneficiary-with-administration — it collects the death-domain receipts and simultaneously runs the machinery that maintains the allocation, so its derived directionality is low but not zero (it bears real maintenance costs). Danka households sit near the target end: they fund both halves across generations with constrained exit; their secondary benefit (complete coverage) moderates the derived value without inverting it. Secular providers are excluded from the allocation entirely rather than positioned within it. Suppression enters the computation as a raw unscaled structural property; only extractiveness is scaled, by directionality and by national scope, which modestly amplifies effective extraction because verification of fair pricing across thousands of temples is difficult.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — two totalizing systems, one population, who takes death — was real, and the partition prevented mislabeling in both directions: it is not a snare, because the coordination is genuine (coverage and inter-institutional peace), and it is not a pure rope, because the death-domain allocation channels persistent transfers that households cannot readily refuse. The mandate has outlived its coercive founding but not yet its function; however, mass secularization is dissolving the 'two systems, one population' premise the arrangement presupposes. The contested founding-problem status paired with a world_rearranges disappearance verdict is recorded honestly here — that combination is exactly the mismatch signal the R5 consumer cross-checks against the computed theater path, and the rising theater series corroborates it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the shinbutsu_ontological_commitment kernel (this reading: partition_reading): does the operative historical commitment match the partition reading, the syncretic reading (honji-suijaku unification), or the incoherence reading (tolerated inconsistency)?',
    'Cross-reading comparison of the three sibling stories plus period evidence distinguishing elite doctrine from lay practice: if honji-suijaku governed actual practice, the syncretic reading wins; if lay behavior tracked domain habit with no metaphysical content, partition or incoherence prevails.',
    'Under the syncretic reading the arrangement is one integrated system with a different beneficiary structure and likely lower measured extraction; under the incoherence reading persistence-without-function points toward piton dynamics. This story''s classification holds only within the partition reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the shinbutsu kernel describes the operative commitment.').

omega_variable(
    emergent_vs_enforced_partition,
    'Is the domain partition an emergent equilibrium sustained by mutual utility, or an enforced allocation whose enforcement machinery has decayed faster than the allocation itself?',
    'Compare pre-Meiji voluntary complementarity, the 1868-1871 coercive imposition, and post-1945 persistence under soft norms: if the partition persists where enforcement is weakest and alternatives are cheapest, it is emergent; if it thins exactly as enforcement recedes, it is enforced-residual.',
    'Emergent supports rope-side certification; enforced-residual predicts continued drift toward piton or snare as enforcement capacity finishes decaying.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergent_vs_enforced_partition, empirical, 'Whether the partition is self-sustaining coordination or residual enforcement.').

omega_variable(
    danka_burden_attribution,
    'Is the funeral-and-grave fee burden borne by households extraction produced BY the domain partition, or by a separate commercial funeral economy that rides on the partition''s jurisdictional allocation?',
    'Decomposition test per the epsilon-invariance rule: author the funeral-fee economy as its own constraint story and re-measure the partition with the fee stream attributed to the child constraint; if the partition''s epsilon collapses toward coordination-cost levels, the burdens belong to the child story.',
    'If separable, this story''s effective extraction drops toward rope range and the fee economy becomes a linked snare-or-tangled-rope story; if inseparable, the partition itself carries the extraction and the tangled_rope claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(danka_burden_attribution, conceptual, 'Whether the danka fee burden belongs to this constraint or to a child funeral-economy constraint.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression keeping households inside Buddhist death rites structural (cemetery control, parish registration, legal friction) or internalized (felt ancestral obligation that persists without enforcement)?',
    'Post-exit trajectory study of households that moved to tree burial, family graves, or secular services: if felt obligation persists after the structural barrier is removed, part of the suppression is internalized.',
    'If substantially internalized, effective suppression exceeds the structural measure and will outlast the cemetery and registration barriers; classification follows the internalized share.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in household death-rite adherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__partition_reading, 1868, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_partition_tr_t1868, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1868, 0.2).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t1868, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t1890, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1890, 0.26).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t1890, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t1920, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1920, 0.32).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t1920, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t1947, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1947, 0.36).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t1947, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t1970, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1970, 0.44).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t1970, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t1995, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1995, 0.52).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t1995, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t2024, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 2024, 0.6).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(shinbutsu_partition_be_t1868, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1868, 0.7).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t1868, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t1890, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1890, 0.56).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t1890, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t1920, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1920, 0.52).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t1920, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t1947, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1947, 0.4).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t1947, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t1970, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1970, 0.46).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t1970, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t1995, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1995, 0.54).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t1995, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t2024, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 2024, 0.46).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_partition_su_t1868, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1868, 0.88).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t1868, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t1890, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1890, 0.62).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t1890, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t1920, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1920, 0.55).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t1920, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t1947, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1947, 0.28).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t1947, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t1970, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1970, 0.34).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t1970, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t1995, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1995, 0.31).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t1995, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t2024, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 2024, 0.24).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__partition_reading, resource_allocation).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Shinto-Buddhist relations' covers three structurally distinct claims about the operative ontological commitment: unified cosmology (syncretic_reading), stable jurisdictional separation without integration (this file), and tolerated inconsistency (incoherence_reading). Each is authored as its own constraint with its own epsilon, beneficiaries, and classification, linked through network edges; this file's values hold only under the partition reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
