% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__domain_partition_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: simultaneous_veneration__domain_partition_reading
 *   human_readable: Simultaneous Kami-Buddha Veneration as Domain Partition (Domain-Partition Reading)
 *   domain: religious/historical
 *
 * SUMMARY:
 *   Pre-Meiji Japan sustained two parallel religious economies — kami shrines
 *   and Buddhist temples — serving the same households for roughly a
 *   millennium. This story authors the domain-partition reading of that
 *   standing arrangement: kami institutions govern this-worldly prosperity
 *   (harvest, health, fortune, purification) and Buddhist institutions govern
 *   death and afterlife (funerals, memorials, salvation), so that venerating
 *   both is not contradiction but division of labor. The reading is claimed
 *   as a coordination arrangement with negligible extraction and no victims;
 *   the metrics are authored independently and, on this reading, they agree —
 *   the domain-partition reading is precisely the reading on which the
 *   arrangement's extraction is near-zero. KEY AGENTS (by structural
 *   relationship): lay_practitioner_households — net beneficiary carrying fee
 *   costs (organized/mobile) — uses both domains' rites and funds both;
 *   kami_shrine_priesthoods — life-domain beneficiary (organized/constrained)
 *   — collects offerings and festival dues; buddhist_temples_and_clergy —
 *   death-domain beneficiary (institutional/constrained) — collects funeral
 *   and memorial fees; court_and_shogunate_patrons — agenda-setter and
 *   legitimation beneficiary (institutional/mobile) — frames and sponsors
 *   both establishments; edo_period_confucian_critics — excluded critic
 *   (moderate/constrained); religious_studies_scholars — analytical observer
 *   (analytical/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__domain_partition_reading, 0.16).
domain_priors:suppression_score(simultaneous_veneration__domain_partition_reading, 0.1).
domain_priors:theater_ratio(simultaneous_veneration__domain_partition_reading, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__domain_partition_reading, rope).
narrative_ontology:human_readable(simultaneous_veneration__domain_partition_reading, "Simultaneous Kami-Buddha Veneration as Domain Partition (Domain-Partition Reading)").
narrative_ontology:topic_domain(simultaneous_veneration__domain_partition_reading, "religious/historical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__domain_partition_reading, 'ec3e0f52-f830-4d9c-8d4e-7698de79e76b').
narrative_ontology:cs_kernel_codification('ec3e0f52-f830-4d9c-8d4e-7698de79e76b', distributed).
narrative_ontology:cs_authority_grounding('ec3e0f52-f830-4d9c-8d4e-7698de79e76b', practice).
narrative_ontology:cs_interpretation_layer_present('ec3e0f52-f830-4d9c-8d4e-7698de79e76b').
narrative_ontology:cs_reading_relation('ec3e0f52-f830-4d9c-8d4e-7698de79e76b', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec3e0f52-f830-4d9c-8d4e-7698de79e76b', simultaneous_veneration__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('ec3e0f52-f830-4d9c-8d4e-7698de79e76b', foundational, kami_buddha_functional_domain_distinctness).
narrative_ontology:cs_axiom_status(kami_buddha_functional_domain_distinctness, holdable).
narrative_ontology:cs_axiom_grounding('ec3e0f52-f830-4d9c-8d4e-7698de79e76b', kami_buddha_functional_domain_distinctness, empirically_contingent).
narrative_ontology:cs_axiom('ec3e0f52-f830-4d9c-8d4e-7698de79e76b', secondary, simultaneous_veneration_as_domain_specialization).
narrative_ontology:cs_axiom_status(simultaneous_veneration_as_domain_specialization, holdable).
narrative_ontology:cs_axiom_grounding('ec3e0f52-f830-4d9c-8d4e-7698de79e76b', simultaneous_veneration_as_domain_specialization, empirically_contingent).
narrative_ontology:cs_reference_frame('ec3e0f52-f830-4d9c-8d4e-7698de79e76b', complementary_domain_partition).
narrative_ontology:cs_drift_state('ec3e0f52-f830-4d9c-8d4e-7698de79e76b', meiji_separation_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('ec3e0f52-f830-4d9c-8d4e-7698de79e76b', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__domain_partition_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, lay_practitioner_households).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, kami_shrine_priesthoods).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, buddhist_temples_and_clergy).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, court_and_shogunate_patrons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(simultaneous_veneration__domain_partition_reading, lay_practitioner_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Households across the archipelago keep both affiliations as a matter of course: they call on shrine priests for purification, harvest festivals, blessings for births and building, and protection from misfortune, and they call on temples for funerals, grave rites, memorial services, and prayers for the dead. They fund both through offerings, festival dues, and service fees, and decide for themselves how much weight to give each side; no authority penalizes a household that favors one. The pattern passes down the family line across generations.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, lay_practitioner_households, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__domain_partition_reading, lay_practitioner_households, payer).

% Hereditary shrine lineages perform the this-worldly rites — purification, festival, divination, blessing — and live on offerings, festival dues, and land income attached to their shrines. Each office is tied to a specific shrine and its patron community; stepping outside the kami-side role would dissolve the office itself. A few great shrines hold national standing; most serve village and town constituencies.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, kami_shrine_priesthoods, beneficiary,
    organized, generational, constrained, national).

% Temple networks ordain clergy, hold land, and conduct the death-side rites — funerals, grave rites, memorial services — along with teaching and prayer for salvation. Funeral and memorial fees are a core revenue line, and temple affiliation gives a household its death ritual for a lifetime. Clergy are bound by ordination and sect rules; sects differ in how much they emphasize kami veneration, but the death-rite function is the floor of the temple economy.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, buddhist_temples_and_clergy, beneficiary,
    institutional, generational, constrained, national).

% The imperial court maintains a separate administrative bureau for kami worship alongside its offices for Buddhist clergy, and successive warrior governments license temples, confirm shrine ranks, and fund state rites in both idioms. Patrons draw legitimation from both establishments — rainmaking and realm-protection rites from the kami side, mortuary and doctrinal authority from the temples — and can reframe the two establishments' relation by edict, as the Meiji government later did from the opposite direction.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, court_and_shogunate_patrons, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__domain_partition_reading, court_and_shogunate_patrons, beneficiary).

% Confucian scholars in samurai service write critiques arguing that both establishments drain the countryside's wealth and that maintaining two cults lacks any single doctrinal ground. They publish and advise, but religious policy is set by the court, the temples, and the shrine lineages; their objections register nowhere in the arrangement's operation. Their class position binds them to the order they criticize.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, edo_period_confucian_critics, excluded,
    moderate, biographical, constrained, national).

% Historians and scholars of religion reconstruct the arrangement from rite calendars, temple and shrine ledgers, doctrinal treatises, and the archive of the Meiji separation. They collect nothing from the arrangement and bear none of its costs; their seat is analytical, assessing whether the domain-partition description fits the recorded practice.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, religious_studies_scholars, observer,
    analytical, generational, analytical, global).

narrative_ontology:fixing_cost_class(simultaneous_veneration__domain_partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement provides lifecycle religious coverage without institutional conflict: kami institutions handle this-worldly concerns (harvest, health, fortune, purification) and Buddhist institutions handle death and afterlife (funerals, memorials, salvation), so two religious economies serve the same households with complementary rather than competing claims.
% TRANSFER_FUNCTION: Moves offerings, festival dues, and funeral and memorial fees from practitioner households to shrine priesthoods for this-worldly rites and to temples for death rites; moves ritual legitimation (realm protection, mortuary authority) upward to the court and warrior governments that sponsor both establishments.
% ABSENT_VOICES: Confucian critics in samurai service objected that dual cult maintenance was ungrounded and costly, and held no seat in the arrangement's operation; Christian missionaries, expelled in the early Edo period, objected from entirely outside that dual veneration was incoherent idolatry. The practitioners' own first-person voices are also largely absent — the consensus this reading describes is inferred from rite calendars, ledgers, and elite testimony rather than practitioner self-report.
% DISAPPEARANCE_RATIONALE: Households would lose either this-worldly or mortuary coverage overnight; the two clergy economies would lose their revenue domains; festival calendars and funeral custom would unravel. The Meiji separation is the natural experiment: legal dissolution required an edict campaign, triggered the destruction of thousands of temples and shrine-temple complexes, and the lifecycle pattern re-formed informally within a generation or two — evidence that the world was arranged around the practice.
% FOUNDING_PROBLEM: How a society that adopts an imported salvation religion (Buddhism, from the sixth century) while maintaining indigenous kami cults can provision the full ritual life of its households — this-worldly fortune and afterlife fate — without the two establishments fighting over the same jurisdiction.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the Meiji state's own separation edicts attest the arrangement was real enough to require forcible dismantling, and modern scholarship of Japanese religion (folklore studies and religious-history literature) attests that the lifecycle partition — kami-side rites for living concerns, temple-side rites for death — persisted after legal separation without enforcement. Neither attestation originates with the arrangement's benefiting parties.
narrative_ontology:disappearance_verdict(simultaneous_veneration__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(simultaneous_veneration__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__domain_partition_reading, 0.16, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__domain_partition_reading_tests).
:- end_tests(simultaneous_veneration__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.16 at interval end) because the arrangement's revenue lines — offerings, festival dues, funeral and memorial fees — track services households demonstrably seek, and no party is bound to either establishment by force. Suppression is authored low (0.10) and is a raw structural property, unscaled by power or scope: nothing enforces dual affiliation, and households favoring one side go unpenalized; only the engine scales extraction, by directionality and scope. Theater is low (0.14): the rites performed are the ones households use, though late-Edo liturgical and doctrinal elaboration adds ornament. Accessibility collapse is low (0.20) because alternatives persist across the whole interval — exclusive kami veneration, exclusively Buddhist practice, selective emphasis, and new movements all operated alongside the dual pattern. Resistance is low (0.08): the partition met sectarian friction at the margins but no sustained opposition to the division of labor itself. The measurement series run on one shared time grid (800, 1100, 1400, 1700, 1868) with every tracked metric authored at every point; suppression_requirement is deliberately not tracked because the enforcement picture is static — the arrangement ran on convention, not enforcement machinery, and the scalar suppression value already carries that fact. The claimed type and the metrics are authored independently; here they agree, and the engine still computes per-seat classifications from the structural data regardless of the claim.
 *
 * PERSPECTIVAL GAP:
 *   From the household seat the arrangement is complementary provision — two institutions, two portfolios, no conflict. From either clergy seat it is a stable franchise boundary guaranteeing each side a revenue domain. From the excluded Confucian seat it is an ungrounded duplication of cult; from the Meiji ideological seat (outside this story's stakeholder set) the same practice was contamination to be purged by force. The two clergy classes sit at comparable institutional standing yet experience different economies: the death-domain side carries a fee-per-service economy concentrated at household crises, the life-domain side a festival-and-offering economy distributed across the calendar — same-level actors differentiated by constraint-specific exit and revenue structure, not by global standing. The engine computes these divergences per seat from power, exit, and role data.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared party is a beneficiary: households receive both domains' rites (their fee costs as secondary position place them slightly above the beneficiary pole toward symmetric), the two clergy classes collect their domains' revenues, and the political patrons collect legitimation. No victim group is declared because, by this reading's lights, no party bears uncompensated costs — the fee economy is the price of services sought, not a transfer under compulsion. Directionality therefore sits near the beneficiary end for the clergy and patron seats and near-symmetric for households; the engine amplifies the modest base extraction by national scope but damps it at every beneficiary seat. No directionality overrides are used: the beneficiary declarations plus exit options already yield the correct values for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two misreadings. Reading the Edo funeral-fee economy as this arrangement's core would convert it into an extraction story with the death domain as captive market; the registration-economy boundary omega holds that reading to a separate decomposition test rather than letting it contaminate this story's epsilon. Reading the arrangement's late persistence as institutional inertia would convert it into a degraded vestige; the post-separation persistence omega tests that directly — the lifecycle partition re-formed without legal support after 1868, which is function, not inertia. The founding-problem interview corroborates from outside the beneficiary set: hostile Meiji ideologues attested the arrangement's reality by the violence of its dismantling, and modern scholarship attests the partition pattern's persistence. The arrangement's mandate did not outlive its function before it was terminated — it was killed, not abandoned — so no mandatrophy resolution is declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'This story instantiates the domain_partition_reading of the simultaneous_veneration kernel. Which reading of that kernel — functional domain partition, ontological fusion, or pragmatic incoherence — best matches the historical arrangement''s structure, and where exactly is the disagreement located?',
    'Comparative coding of practice records (household rite calendars, shrine and temple ledgers, funeral registers) against doctrinal treatises across the interval; the reading whose beneficiary structure and extraction profile the practice records support prevails.',
    'The ontological-fusion sibling would re-author epsilon around a hierarchical kami-to-buddha relation and likely raise extractiveness; the pragmatic-incoherence sibling would raise suppression (contradiction sustained without enforcement pressure) and push the Meiji endpoint toward degraded or extractive dynamics. This file authors only the partition reading''s epsilon-invariant constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Committer-frame omega: one reading of the simultaneous_veneration kernel; sibling readings would reclassify the same standing arrangement.').

omega_variable(
    cultic_vs_ontological_distinctness,
    'Does this reading''s claim that kami and buddhas are distinct entities operate at the cultic-functional level (separate cults with separate portfolios, compatible with theories of ontological identity) or the ontological level (two kinds of beings, which would logically foreclose the fusion reading within any single framework)?',
    'Test whether mainstream pre-Meiji frameworks held functional partition and ontological identity simultaneously; if such frameworks were the norm, the distinctness claim is cultic-functional and the fusion reading is not foreclosed.',
    'If the claim is ontological, this reading forecloses the fusion sibling and the kernel contest is logical; if cultic-functional, both readings remain live and the contest is empirical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultic_vs_ontological_distinctness, conceptual, 'Location of the foreclosure question between the partition and fusion readings.').

omega_variable(
    edo_death_economy_boundary,
    'Where does the domain-partition arrangement end and the Edo compulsory temple-registration economy begin — is the professionalized Tokugawa funeral-fee economy part of this arrangement or a distinct constraint?',
    'Decomposition test: author the registration mechanism (the parishioner-household system binding households to temples under state requirement) as its own story; if its extraction is substantially higher and driven by state compulsion rather than domain logic, the boundary is real.',
    'If the registration economy is inside this arrangement, death-domain epsilon rises toward 0.25+ and the type drifts toward a hybrid coordination-extraction structure; if outside, this story keeps low epsilon and the sibling story carries the extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(edo_death_economy_boundary, empirical, 'Epsilon-invariance boundary between the partition arrangement and the Edo registration-backed funeral economy.').

omega_variable(
    life_death_domain_epsilon_split,
    'Do the life-domain (kami) and death-domain (buddha) sides of the partition carry independent epsilon values, as the expected structural delta specifies — and if so, what are they?',
    'Author the two sub-constraints separately (a kami life-domain arrangement story and a buddha death-domain arrangement story) and measure each side''s fee structure and compulsion independently.',
    'If the death domain''s epsilon is substantially higher (funeral-fee economy concentrated at household crises), the joint value of 0.16 understates the death side and overstates the life side; the family should be split into two linked stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(life_death_domain_epsilon_split, empirical, 'The reading''s internal two-constraint decomposition with independent epsilon values.').

omega_variable(
    post_separation_persistence,
    'Does the lifecycle partition pattern persist after the arrangement''s legal dissolution without enforcement — and does that persistence confirm genuine coordination function over institutional inertia?',
    'Post-1868 practice evidence: continuation of kami-side rites for living concerns and temple-side rites for death absent any legal requirement, as documented in folklore studies and religious-history surveys.',
    'Persistence without enforcement supports the coordination reading (self-organizing allocation of religious demand); rapid decay after 1868 would instead have supported an inertia reading of the late-Edo arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(post_separation_persistence, empirical, 'Rope-versus-inertia test from the arrangement''s behavior after forced dissolution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__domain_partition_reading, 800, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t800, simultaneous_veneration__domain_partition_reading, theater_ratio, 800, 0.06).
narrative_ontology:measurement_basis(simu_tr_t800, observed).
narrative_ontology:measurement(simu_tr_t1100, simultaneous_veneration__domain_partition_reading, theater_ratio, 1100, 0.08).
narrative_ontology:measurement_basis(simu_tr_t1100, observed).
narrative_ontology:measurement(simu_tr_t1400, simultaneous_veneration__domain_partition_reading, theater_ratio, 1400, 0.1).
narrative_ontology:measurement_basis(simu_tr_t1400, observed).
narrative_ontology:measurement(simu_tr_t1700, simultaneous_veneration__domain_partition_reading, theater_ratio, 1700, 0.12).
narrative_ontology:measurement_basis(simu_tr_t1700, observed).
narrative_ontology:measurement(simu_tr_t1868, simultaneous_veneration__domain_partition_reading, theater_ratio, 1868, 0.14).
narrative_ontology:measurement_basis(simu_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(simu_be_t800, simultaneous_veneration__domain_partition_reading, base_extractiveness, 800, 0.08).
narrative_ontology:measurement_basis(simu_be_t800, observed).
narrative_ontology:measurement(simu_be_t1100, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1100, 0.1).
narrative_ontology:measurement_basis(simu_be_t1100, observed).
narrative_ontology:measurement(simu_be_t1400, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1400, 0.13).
narrative_ontology:measurement_basis(simu_be_t1400, observed).
narrative_ontology:measurement(simu_be_t1700, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1700, 0.16).
narrative_ontology:measurement_basis(simu_be_t1700, observed).
narrative_ontology:measurement(simu_be_t1868, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1868, 0.16).
narrative_ontology:measurement_basis(simu_be_t1868, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(simultaneous_veneration__domain_partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__domain_partition_reading, resource_allocation).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% Constraint family of the simultaneous_veneration kernel: this domain-partition reading, the ontological-fusion reading, and the pragmatic-incoherence reading are sibling readings of one standing arrangement (pre-Meiji dual veneration), linked because each authors a different epsilon over the same referent. This reading further decomposes into two parallel sub-constraints — the life-domain kami arrangement and the death-domain buddha arrangement — with independent epsilon values (life-domain near zero; death-domain modestly higher via the funeral-fee economy); that split is documented in the life_death_domain_epsilon_split omega and should be authored as separate linked stories if finer resolution is needed. The kernel contest itself is routed to the kernel_reading_selection omega, not adjudicated inside this file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
