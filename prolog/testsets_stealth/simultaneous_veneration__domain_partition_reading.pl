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
 *   constraint_id: simultaneous_veneration__domain_partition_reading
 *   human_readable: Kami-Buddha Domain Partition: This-Worldly vs Afterlife Jurisdictions
 *   domain: religious/comparative-religion/japanese-history
 *
 * SUMMARY:
 *   From roughly the Nara consolidation through the Meiji separation edicts,
 *   Japanese religious practice ran a dual economy: kami shrines handled
 *   this-worldly traffic (harvest, health, protection, prosperity) and
 *   Buddhist temples handled death traffic (funerals, memorial cycles,
 *   rebirth), with lay practitioners moving between them along a routing
 *   heuristic nobody experienced as contradiction. This file instantiates the
 *   domain_partition_reading of the simultaneous_veneration kernel as a
 *   clean, epsilon-invariant constraint: the standing arrangement under
 *   contest, assessed by this reading's own lights, is domain-appropriate
 *   specialization — real services exchanged for real support, with no
 *   identifiable victim class. Per the expected structural delta, the reading
 *   decomposes into two parallel sub-constraints (a life-domain kami cult and
 *   a death-domain buddha cult) with independent epsilon values; those
 *   sub-stories are linked in network.affects_constraints rather than folded
 *   into this file. The claimed_type (rope) reflects what I believe is
 *   structurally true of the arrangement; the metric values reflect what I
 *   believe is descriptively true of its operation, authored independently —
 *   their convergence here is a substantive finding of this reading, not a
 *   tuning outcome.
 *
 * KEY AGENTS:
 *   - - lay_practitioners: Primary participant-beneficiaries (moderate/mobile) — receive domain-appropriate services from both sides, pay ordinary service prices
 *   - - shrine_priesthood: Kami-side administrator-beneficiary (organized/constrained) — holds the this-worldly jurisdiction the partition secures
 *   - - buddhist_clergy: Death-side administrator-beneficiary (institutional/constrained) — holds the non-deferrable afterlife portfolio
 *   - - imperial_court: Codifying agenda-setter (institutional/constrained) — set the integrated framework and consumed its legitimation
 *   - - women_barred_from_precincts: Excluded seat (powerless/constrained) — bears the boundary-drawing without having shaped it
 *   - - comparative_religion_historians: Analytical observer (analytical/analytical) — sees the full structure from outside participant commitments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__domain_partition_reading, 0.15).
domain_priors:suppression_score(simultaneous_veneration__domain_partition_reading, 0.1).
domain_priors:theater_ratio(simultaneous_veneration__domain_partition_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__domain_partition_reading, rope).
narrative_ontology:human_readable(simultaneous_veneration__domain_partition_reading, "Kami-Buddha Domain Partition: This-Worldly vs Afterlife Jurisdictions").
narrative_ontology:topic_domain(simultaneous_veneration__domain_partition_reading, "religious/comparative-religion/japanese-history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__domain_partition_reading, 'def95deb-eb8c-42ad-bb11-6bd73c2da54a').
narrative_ontology:cs_kernel_codification('def95deb-eb8c-42ad-bb11-6bd73c2da54a', distributed).
narrative_ontology:cs_authority_grounding('def95deb-eb8c-42ad-bb11-6bd73c2da54a', practice).
narrative_ontology:cs_interpretation_layer_present('def95deb-eb8c-42ad-bb11-6bd73c2da54a').
narrative_ontology:cs_reading_relation('def95deb-eb8c-42ad-bb11-6bd73c2da54a', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('def95deb-eb8c-42ad-bb11-6bd73c2da54a', simultaneous_veneration__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('def95deb-eb8c-42ad-bb11-6bd73c2da54a', foundational, kami_buddha_functional_distinction).
narrative_ontology:cs_axiom_status(kami_buddha_functional_distinction, holdable).
narrative_ontology:cs_axiom_grounding('def95deb-eb8c-42ad-bb11-6bd73c2da54a', kami_buddha_functional_distinction, conventional).
narrative_ontology:cs_axiom('def95deb-eb8c-42ad-bb11-6bd73c2da54a', foundational, joint_veneration_is_coherent_specialization).
narrative_ontology:cs_axiom_status(joint_veneration_is_coherent_specialization, holdable).
narrative_ontology:cs_axiom_grounding('def95deb-eb8c-42ad-bb11-6bd73c2da54a', joint_veneration_is_coherent_specialization, instrumental).
narrative_ontology:cs_reference_frame('def95deb-eb8c-42ad-bb11-6bd73c2da54a', complementary_jurisdiction_dual_cult).
narrative_ontology:cs_drift_state('def95deb-eb8c-42ad-bb11-6bd73c2da54a', contemporary_post_separation_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('def95deb-eb8c-42ad-bb11-6bd73c2da54a', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__domain_partition_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, lay_practitioners).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, shrine_priesthood).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, buddhist_clergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Farm households, townspeople, and elites who visit shrines for festivals, purification, and prayers for harvest, health, and commercial success, and turn to temples for funerals, memorial services, and reassurance about rebirth. They pay offerings and service fees to both institutions and experience the payments as the ordinary price of services rendered, not as tribute. Nothing prevents them from weighting one side more heavily, joining a Pure Land confraternity, or adding ascetic practices; the arrangement channels demand rather than compelling it.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, lay_practitioners, beneficiary,
    moderate, biographical, mobile, national).

% Hereditary custodial lines administering the kami side of the partition: purification rites, festivals, divination, and prayers for this-worldly outcomes. The domain assignment secures their jurisdiction — this-worldly traffic belongs to them and temples do not compete for it — and their offerings and prestige depend on that boundary holding. Exit is limited by hereditary attachment to specific shrines and their landed patron networks.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, shrine_priesthood, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__domain_partition_reading, shrine_priesthood, agenda_setter).

% Monastic corporations holding the death portfolio: funerals, memorial cycles, sutra rites for the dead, and Pure Land practices. Death services are non-deferrable — every household eventually needs them — which gives temples steady demand that shrine festival traffic lacks. Major temples accumulated estates and political weight across the interval. Ordination and monastic career paths bind individuals to their institutions; the partition shields their death-side monopoly from kami-side competition.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, buddhist_clergy, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__domain_partition_reading, buddhist_clergy, agenda_setter).

% Codified the integrated framework from the Nara and Heian periods onward: ordering joint rites, sponsoring shrine-temple combinations, and treating the dual cult as realm-protecting infrastructure. The court consumed legitimation from both sides — kami protecting the land, Buddhism protecting the state — and could not exit the arrangement without surrendering that dual source of legitimacy. Its direct administrative role receded over the interval even as the framework it set persisted.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, imperial_court, agenda_setter,
    institutional, generational, constrained, national).

% Women subject to exclusion zones around certain inner sanctums and mountain summits, practicing vicariously through male relatives or from boundary halls. They bore the arrangement's boundary-drawing in a way the beneficiary seats did not, and would contest the terms of the partition — which spaces, which rites, which domains admit them — if they were seated in the conversation that maintained it.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, women_barred_from_precincts, excluded,
    powerless, biographical, constrained, regional).

% Reconstruct the arrangement from diaries, temple registers, festival records, and doctrinal texts; they see the full structure — the routing heuristic, the two clerical economies, the court's legitimation consumption, and the boundary disputes — from outside any participant's commitments.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, comparative_religion_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__domain_partition_reading, diffuse).
narrative_ontology:fixing_cost_class(simultaneous_veneration__domain_partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Routes devotional demand between two specialist institutions: this-worldly concerns (harvest, health, protection, prosperity) go to kami shrines; death and afterlife concerns (funerals, memorials, rebirth) go to Buddhist temples. This solves the practitioner's allocation problem — whom to approach for what — and prevents the two clergies from colliding over the same ritual market.
% TRANSFER_FUNCTION: Moves offerings, festival labor, and death-service fees from lay households to shrine and temple corporations; moves protective rites and salvific services back to households; and moves legitimation upward to the court, which consumed the dual cult's realm-protecting symbolism.
% ABSENT_VOICES: Women barred from inner precincts and summits would object to how the partition drew its boundaries — they lived at the exclusion lines the beneficiary seats never crossed. Unaffiliated ascetics operating outside the parish-shrine structure would object to the arrangement's assumption that all religious traffic flows through the two licensed channels.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished overnight, the devotional economy would immediately need reorganization: practitioners would lose the routing heuristic that told them whom to approach, the two clergies would either collide over overlapping jurisdictions or merge, and the festival-and-funeral calendar that structured village and household life would unravel. The coordination problem the partition solved would reappear at once.
% FOUNDING_PROBLEM: Integrating an arriving salvific, textual religion (Buddhism) with an indigenous this-worldly cult (kami) without forcing either to absorb or abolish the other — allocating jurisdiction so both could operate and both their clienteles could be served.
% FOUNDING_PROBLEM_CORROBORATION: No single seat attests the status uncontested. Kokugaku scholars of the Edo period and Meiji-state ideologues — both outside the benefiting clergy — attested that the founding problem had been superseded or falsified, arguing the integrated arrangement had become a corrupt overlay obscuring the original kami way. Descendant practice communities and the persistence of divided habits (shrine festivals, temple funerals) attest the integrative problem as still live. The status is genuinely disputed across seats, and the dispute is documented in kokugaku writings and Meiji separation-edict deliberations rather than asserted by the arrangement's own beneficiaries alone.
narrative_ontology:disappearance_verdict(simultaneous_veneration__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__domain_partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__domain_partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(simultaneous_veneration__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__domain_partition_reading, 0.15, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is low (0.15 at interval end) because the arrangement's transfers are service-priced: offerings and funeral fees buy deliverable goods (rites, festivals, mourning structure), and the slow rise across the interval tracks clerical wealth accumulation and late-period fee formalization rather than growing tribute. Suppression is low (0.10) and static — the partition persisted by complementarity and habit, not by enforcement machinery, so no suppression_requirement series is authored; the static picture is carried by the scalar. Theater is low (0.12): whatever one thinks of the metaphysics, the rituals delivered functioning services — festivals coordinated villages, funerals structured grief — so performative overhead stayed minor even as ritual elaborated in the Edo period. Accessibility_collapse is moderate (0.35): alternatives remained live throughout — exclusive kami traditionalism, Pure Land exclusivism, mountain asceticism, briefly Christianity — because the partition channeled demand rather than foreclosing exits. Resistance is modest (0.25): episodic shrine-house resentment of temple dominance and the Edo-period kokugaku critique, which ultimately supplied the intellectual ammunition for the Meiji separation. The measurement series share one six-point grid so every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the practitioner seat the arrangement reads as ordinary exchange — two service providers, two needs, no tribute. From the two clerical seats it reads as secured jurisdiction: each collects a protected revenue stream the partition guarantees against the other's competition. From the court seat it reads as legitimation infrastructure it codified and depends on. From the excluded women's seat the same boundary-drawing that looks like orderly specialization to the beneficiaries looks like a barrier they were never consulted on. The two clergies are the sharpest same-level contrast: nominally parallel institutional actors with materially different demand profiles, because death services are non-deferrable while festival traffic is discretionary — the engine derives their differing directionalities from the structural data, not from their equal nominal standing.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared party sits near the beneficiary end of the directionality axis: lay_practitioners receive the services the arrangement exists to deliver; the two clergies collect protected jurisdictions; the court consumes legitimation. No victim group is declared because, under this reading, none exists — that absence is precisely what separates this rope from a tangled_rope, and it is a substantive claim of the reading, not an omission. The excluded women's seat carries real grievance, but per the R3 ruling an authored absence drives no classification override; their situation is recorded as testimony, and the omega variable on suppression attribution tracks whether their exclusion belongs to this constraint or to adjacent purity norms. Effective extraction is therefore damped toward subsidy for nearly all seats, with scope amplification modest at national scale.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — integrating an immigrant salvific religion with an indigenous cult by splitting jurisdictions — remained functionally live for the entire thousand-year interval; the arrangement was terminated from outside by revolutionary state action (the 1868 separation edicts and subsequent anti-Buddhist campaigns), not by internal atrophy. There is no theatrical maintenance regime, no administrator keeping a dead function alive for show, and no cost-asymmetry signature of a piton: the arrangement died because a coalition decided killing it was worth revolutionary cost, which is the opposite of neglect. mandatrophy_resolved is accordingly false, and the R5 mismatch consumer finds status=contested paired with verdict=world_rearranges — no dead-mandate flag. The classification prevents mislabeling in both directions: it blocks the kokugaku/Meiji reading that the arrangement was always covert extraction (which would make it a snare), and it blocks the sentimental reading that a millennium-old arrangement must therefore have been frictionless (the slow extractiveness drift and the excluded women's seat are recorded against that).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the kernel simultaneous_veneration (reading: domain_partition_reading). Which reading is instantiated changes the constraint itself: what would the sibling readings (ontological_fusion_reading, pragmatic_incoherence_reading) alter structurally?',
    'Comparative analysis across the three linked stories: the engine computes per-seat classifications for each reading independently, and the corpus compares their epsilon, suppression, and stakeholder surfaces as alternative instantiations of the same practice-kernel.',
    'The fusion reading likely adds a doctrine-enforcement layer (raising suppression moderately); the incoherence reading likely raises theater and suppression substantially (contradiction sustained by absent pressure). This file''s low-extraction rope profile is conditional on the partition reading being the correct instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: which reading of the shared kernel this constraint instantiates, and the structural delta each sibling would introduce.').

omega_variable(
    subconstraint_epsilon_independence,
    'The reading decomposes into two parallel sub-constraints — a life-domain kami cult and a death-domain buddha cult — with independent epsilon values. Does the unified epsilon of 0.15 mask divergent extraction between the halves?',
    'Author the two sub-stories separately (kami_life_domain_cult, buddha_death_domain_cult) and compare their extracted values; test whether death-side service fees extract more than this-worldly offering traffic.',
    'If the death portfolio''s epsilon substantially exceeds the life portfolio''s, the aggregate rope classification conceals asymmetric extraction concentrated in the non-deferrable death-services market, and the partition reading''s benignity claim narrows to the life domain only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subconstraint_epsilon_independence, empirical, 'Whether the two halves of the partition carry independent, potentially divergent extraction levels.').

omega_variable(
    coercive_overlay_contamination,
    'How much of the late-interval extraction and suppression belongs to the domain-partition norm itself, versus coercive overlays riding on it — the Tokugawa parish-registration system binding households to temples, and the Meiji separation machinery that destroyed the arrangement?',
    'Decompose and attribute: measure extraction attributable to the routing norm and its service economy versus the registration state''s use of temple affiliation, using the linked overlay stories as controls.',
    'If the overlays account for most late-period extraction, this constraint''s epsilon stays flat-low across the interval and the gentle rise in the measurement series is reattributed; if the partition norm itself thickened into compulsory parish obligation, the rope classification erodes toward tangled_rope in the final century.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercive_overlay_contamination, empirical, 'Attribution of late-interval extraction between the partition norm and adjacent coercive institutions.').

omega_variable(
    excluded_women_suppression_attribution,
    'Does the exclusion of women from inner precincts and summits belong to this constraint''s suppression profile, or to adjacent sacred-purity and gender constraints that would operate under any jurisdictional arrangement?',
    'Compare exclusion patterns across arrangements with and without the domain partition, and against the linked gender-exclusion constraint stories; test whether exclusion lines track the kami-buddha boundary or sacred-space purity norms generally.',
    'If attributed to this constraint, suppression rises well above rope-typical levels and the beneficiary-only declaration becomes incomplete; if attributed elsewhere, this file''s low suppression stands and the excluded seat''s grievance is carried by the adjacent stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_women_suppression_attribution, conceptual, 'Whether the excluded women''s seat loads suppression onto this constraint or onto neighboring purity constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__domain_partition_reading, 800, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sv_dpr_tr_t800, simultaneous_veneration__domain_partition_reading, theater_ratio, 800, 0.05).
narrative_ontology:measurement_basis(sv_dpr_tr_t800, observed).
narrative_ontology:measurement(sv_dpr_tr_t1050, simultaneous_veneration__domain_partition_reading, theater_ratio, 1050, 0.07).
narrative_ontology:measurement_basis(sv_dpr_tr_t1050, observed).
narrative_ontology:measurement(sv_dpr_tr_t1300, simultaneous_veneration__domain_partition_reading, theater_ratio, 1300, 0.09).
narrative_ontology:measurement_basis(sv_dpr_tr_t1300, observed).
narrative_ontology:measurement(sv_dpr_tr_t1550, simultaneous_veneration__domain_partition_reading, theater_ratio, 1550, 0.1).
narrative_ontology:measurement_basis(sv_dpr_tr_t1550, observed).
narrative_ontology:measurement(sv_dpr_tr_t1700, simultaneous_veneration__domain_partition_reading, theater_ratio, 1700, 0.12).
narrative_ontology:measurement_basis(sv_dpr_tr_t1700, observed).
narrative_ontology:measurement(sv_dpr_tr_t1868, simultaneous_veneration__domain_partition_reading, theater_ratio, 1868, 0.12).
narrative_ontology:measurement_basis(sv_dpr_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(sv_dpr_be_t800, simultaneous_veneration__domain_partition_reading, base_extractiveness, 800, 0.06).
narrative_ontology:measurement_basis(sv_dpr_be_t800, observed).
narrative_ontology:measurement(sv_dpr_be_t1050, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1050, 0.09).
narrative_ontology:measurement_basis(sv_dpr_be_t1050, observed).
narrative_ontology:measurement(sv_dpr_be_t1300, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1300, 0.11).
narrative_ontology:measurement_basis(sv_dpr_be_t1300, observed).
narrative_ontology:measurement(sv_dpr_be_t1550, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1550, 0.12).
narrative_ontology:measurement_basis(sv_dpr_be_t1550, observed).
narrative_ontology:measurement(sv_dpr_be_t1700, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1700, 0.14).
narrative_ontology:measurement_basis(sv_dpr_be_t1700, observed).
narrative_ontology:measurement(sv_dpr_be_t1868, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1868, 0.15).
narrative_ontology:measurement_basis(sv_dpr_be_t1868, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(simultaneous_veneration__domain_partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__domain_partition_reading, resource_allocation).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, kami_life_domain_cult).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, buddha_death_domain_cult).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, danka_parish_registration).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, meiji_shinbutsu_bunri_separation).

% DUAL FORMULATION NOTE:
% Constraint family for the simultaneous_veneration kernel. This file is the domain_partition_reading; the expected structural delta decomposes it into two parallel sub-constraints — kami_life_domain_cult (this-worldly jurisdiction) and buddha_death_domain_cult (afterlife jurisdiction) — each carrying an independent epsilon; this story links both and defers their separate measurement to their own files. danka_parish_registration is a downstream overlay that bolted compulsory household affiliation onto the death-side channel in the Edo period, and meiji_shinbutsu_bunri_separation is the terminal event that dismantled the arrangement in 1868. The sibling readings of the same kernel (ontological_fusion_reading, pragmatic_incoherence_reading) are related through cs_structure.reading_relations, not through this network: they are alternative instantiations of one kernel, not upstream or downstream constraints. Upstream/downstream citation flow runs from this partition reading to the fusion reading's doctrinal apparatus, since functional specialization arguments were routinely absorbed into honji-suijaku elaborations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
