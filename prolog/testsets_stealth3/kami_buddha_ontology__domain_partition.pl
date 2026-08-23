% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__domain_partition
% ============================================================================
% Version: 7.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__domain_partition, []).

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
 *   constraint_id: kami_buddha_ontology__domain_partition
 *   human_readable: Domain Partition of Kami and Buddhas (Shinbutsu Bunri Reading)
 *   domain: religious studies/japanese cultural history
 *
 * SUMMARY:
 *   The domain-partition claim, that kami and buddhas are ontologically
 *   distinct agents with complementary non-overlapping jurisdictions,
 *   operates in Japanese religious life as an arrangement with two strata: an
 *   old folk-practical division of ritual labor, and a Meiji-enforced
 *   separation that dismantled centuries of institutional fusion and then
 *   decayed into custom after 1945. The arrangement solves a genuine
 *   allocation problem while carrying asymmetric, actively maintained
 *   extraction: a death-jurisdiction monopoly monetized through hereditary
 *   parish obligations, purchased at the price of destroying fused practice.
 *   Claim and metrics are authored independently: I claim tangled_rope
 *   because both halves are structurally present; the metrics describe what
 *   the arrangement actually does, and the engine computes per-seat verdicts
 *   from the structural data. The epsilon referent is the standing
 *   partitioned arrangement across both strata, assessed by this reading's
 *   own lights.
 *
 * KEY AGENTS:
 *   - - meiji_nationalist_ideologues: Agenda-setting authors of the separation program (institutional/arbitrage) — designed the partition and departed after implementation
 *   - - home_ministry_shrine_bureau: Enforcing administrator (institutional/mobile) — ran inspections, destruction orders, and clergy sanctions
 *   - - buddhist_temple_establishment: Primary beneficiary with a historical victim phase (organized/constrained) — holds the death-jurisdiction side and receives the recurring fee base
 *   - - shrine_priesthood: Secondary beneficiary (organized/constrained) — holds the purity-jurisdiction side of the partition
 *   - - danka_households: Principal continuing payers (powerless/constrained) — bear hereditary fees with service offsets
 *   - - fused_village_communities: Principal historical victims (powerless/trapped) — bore the dismantling of integrated sacred sites
 *   - - jinguji_institutions: Abolished intermediary institutions (powerless/trapped) — the corporate form destroyed by enforcement
 *   - - shinto_death_rite_advocates: Excluded voice (moderate/constrained) — barred from the death jurisdiction the partition reserves
 *   - - religious_studies_scholars: Analytical observer (analytical/analytical) — sees the full structure across all kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, 0.52).
domain_priors:suppression_score(kami_buddha_ontology__domain_partition, 0.34).
domain_priors:theater_ratio(kami_buddha_ontology__domain_partition, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, extractiveness, 0.52).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__domain_partition, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__domain_partition, "Domain Partition of Kami and Buddhas (Shinbutsu Bunri Reading)").
narrative_ontology:topic_domain(kami_buddha_ontology__domain_partition, "religious studies/japanese cultural history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__domain_partition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__domain_partition, '9fce5018-ff56-43f6-a491-692178250d3e').
narrative_ontology:cs_kernel_codification('9fce5018-ff56-43f6-a491-692178250d3e', distributed).
narrative_ontology:cs_authority_grounding('9fce5018-ff56-43f6-a491-692178250d3e', practice).
narrative_ontology:cs_interpretation_layer_present('9fce5018-ff56-43f6-a491-692178250d3e').
narrative_ontology:cs_reading_relation('9fce5018-ff56-43f6-a491-692178250d3e', kami_buddha_ontology__honji_suijaku_monism, forecloses).
narrative_ontology:cs_reading_relation('9fce5018-ff56-43f6-a491-692178250d3e', kami_buddha_ontology__incoherent_bundle, coexists_with).
narrative_ontology:cs_axiom('9fce5018-ff56-43f6-a491-692178250d3e', foundational, kami_buddha_ontological_distinctness).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_distinctness, holdable).
narrative_ontology:cs_axiom_grounding('9fce5018-ff56-43f6-a491-692178250d3e', kami_buddha_ontological_distinctness, theological).
narrative_ontology:cs_axiom('9fce5018-ff56-43f6-a491-692178250d3e', secondary, death_pollution_barred_from_kami_sites).
narrative_ontology:cs_axiom_status(death_pollution_barred_from_kami_sites, holdable).
narrative_ontology:cs_axiom_grounding('9fce5018-ff56-43f6-a491-692178250d3e', death_pollution_barred_from_kami_sites, conventional).
narrative_ontology:cs_reference_frame('9fce5018-ff56-43f6-a491-692178250d3e', parallel_domain_dualism).
narrative_ontology:cs_drift_state('9fce5018-ff56-43f6-a491-692178250d3e', contemporary_postdisestablishment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9fce5018-ff56-43f6-a491-692178250d3e', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__domain_partition, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, buddhist_temple_establishment).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, shrine_priesthood).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, meiji_nationalist_ideologues).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, fused_village_communities).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, jinguji_institutions).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, danka_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, danka_households).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, buddhist_temple_establishment).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, shrine_priesthood).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A coalition of nativist scholars, court officials, and state builders who argued that sovereign national cult required purging Buddhist elements from shrine worship. They drafted the 1868 separation edicts, staffed the short-lived Doctrine Ministry, and supplied the one-nation-one-ancestral-cult justification for dismantling combined shrine-temple complexes. Once the policy was implemented they moved to other projects; their standing never depended on administering what they had built.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, meiji_nationalist_ideologues, agenda_setter,
    institutional, generational, arbitrage, national).

% The ministry office that administered the separation campaign: issuing edict circulars, dispatching inspectors to certify that shrines contained no Buddhist objects, ordering removal or burning of Buddhist images from shrine grounds, laicizing shrine-attached clerics, and later ranking shrines and assigning state stipends. It worked through prefectural governors and could delist or defrock noncompliant clergy.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, home_ministry_shrine_bureau, agenda_setter,
    institutional, biographical, mobile, national).

% The surviving temple corporations and their denominational headquarters. During the persecution years they lost lands, images, and parishioners, and thousands of temples were merged or closed outright. After the pressure eased they rebuilt around the one ritual jurisdiction left uncontested to them: funerals, graves, and memorial services. Household parish registration gives them a recurring fee base of funeral honoraria, grave upkeep, and anniversary offerings, and denominational literature supplies the justification for exclusive death jurisdiction. Families can sever the registration, but doing so is socially costly, which keeps revenue stable even as explicit belief declines.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, buddhist_temple_establishment, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, buddhist_temple_establishment, payer).

% Hereditary and licensed priests of the shrine system. Separation handed them exclusive claim over purity-managed rites: births, newborn presentations, land purification, festival life, and for decades state stipends backed by compulsory parish rolls. They simultaneously lost what their former temple attachments had provided, chiefly funeral income and death-anniversary offerings. Professional formation treats contact with the dead as defiling, so re-entering the funeral market would require overturning their own purity discipline.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, shrine_priesthood, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, shrine_priesthood, payer).

% Ordinary families registered with a neighborhood temple. They receive funerals, gravesites, and annual memorial services, and owe recurring payments: funeral honoraria that run high relative to comparable ceremonies, grave maintenance fees, and segmental anniversary offerings. Registration is inherited along with the family grave; dropping it strains kin relations and complicates burial logistics. Many report thin personal attachment to Buddhist doctrine alongside uninterrupted payment.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, danka_households, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, danka_households, beneficiary).

% Rural communities whose sacred life ran through combined complexes hosting kami festivals, Buddhist mortuary halls, and shared rites honoring both. The edict campaign forced them to split these sites, surrender images, and choose affiliations; villages that resisted saw leaders fined or prosecuted. Their descendants inherit a landscape of divided institutions and fragmented festival calendars, with the pre-separation liturgy carried mainly by elders and local historians.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, fused_village_communities, payer,
    powerless, generational, trapped, regional).

% The shrine-attached temples themselves, the monastic complexes embedded in shrine precincts that had mediated between kami and buddha cults for centuries. Between 1868 and 1871 they were abolished wholesale: buildings demolished, statues burned or sold, resident clerics laicized or expelled. As corporate actors they ceased to exist; their traces survive in local records and in the split physical fabric of shrine precincts.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, jinguji_institutions, payer,
    powerless, generational, trapped, regional).

% Priests, sectarian groups, and reformers who contend that kami cults can serve the dead, citing precedents where shrines handled mortuary rites before purity doctrine hardened. They publish liturgies and perform occasional shrine funerals, but face a market arranged against them: entrenched temple incumbency in parish networks, defilement norms inside the priesthood itself, and funeral-industry defaults. Their share of the death-rite market remains marginal despite periodic revival efforts.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, shinto_death_rite_advocates, excluded,
    moderate, biographical, constrained, national).

% Academic specialists in Japanese religion, historians of kami-buddha relations and sociologists of contemporary practice. They document the pre-separation fusion, the mechanics of the nineteenth-century campaign, and current statistics on funeral affiliation, and they generate the competing interpretive frames gathered under this story's kernel. They collect no revenue from the arrangement and their analyses carry no jurisdictional force.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, religious_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__domain_partition, buddhist_temple_establishment).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__domain_partition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates ritual jurisdiction over the human lifecycle between two sacerdotal systems serving the same population: purity-managed life transitions and agricultural cycles to kami shrines, death, burial, and posthumous memorial care to Buddhist temples. Without the allocation, both systems claim the same rites and households face duplicate obligations and open jurisdictional conflict.
% TRANSFER_FUNCTION: Moves money and obligation from lay households to institutional religion: funeral honoraria, grave maintenance, anniversary offerings, and parish dues flow to temples; purification fees, offerings, and historically compulsory parish support flow to shrines. In the enforcement era it also moved property, with fused sacred assets reassigned to whichever side survived, and moved legitimation upward to the state in exchange for administrative recognition.
% ABSENT_VOICES: The villagers whose combined complexes were dismantled had no seat in the 1868 to 1871 edict process; petitions and rural uprisings against separation were met with prosecution rather than representation. Shinto death-rite advocates remain outside the death jurisdiction the partition assigns. And the parties whose identities the partition defines, the kami and buddhas as worshipped, enter only through institutions claiming to speak for them; no seat represents the fused practice itself.
% DISAPPEARANCE_RATIONALE: Funeral and grave arrangements are contractually and emotionally load-bearing: if the death-jurisdiction assignment vanished overnight, parish revenue streams, cemetery-law defaults, funeral-industry routing, and family memorial calendars would all reorganize; shrines would face immediate demand for death rites they currently refuse on purity grounds; and the two-clergy division of the lifecycle calendar, new year visits, childhood blessings, weddings, funerals, and memorial anniversaries, would lose its coordinating frame.
% FOUNDING_PROBLEM: Two layered problems. The older stratum: keeping death pollution away from purity-managed sacred space so kami rites remain performable, an allocation problem inherent to operating purity and death cults for one population. The Meiji stratum: constructing a sovereign national cult purged of foreign contamination, breaking Buddhist institutional power over household registration and funerals, and anchoring imperial ideology in an exclusively kami-centered ancestry.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary institutions: occupation-era disestablishment analysis and the religious-studies literature on pre-modern fusion and State Shinto corroborate the two-stratum genealogy; prefectural archives and village histories document that separation was imposed against recorded local resistance rather than demanded by practitioners; and market evidence, the steady rise of nonreligious funerals to roughly a third of the total, indicates the death-jurisdiction problem no longer uniquely requires the temple solution. No party outside the beneficiary set attests that the nationalist founding problem remains live.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__domain_partition, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__domain_partition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__domain_partition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kami_buddha_ontology__domain_partition, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__domain_partition, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__domain_partition_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__domain_partition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   End-state extractiveness sits at 0.52 because the standing arrangement mixes priced service (funerals and memorials households would purchase in some form regardless) with a partition premium: hereditary parish lock-in, honoraria inflated relative to comparable ceremonies, and foreclosure of the fused alternative. Suppression is 0.34: state coercion collapsed at disestablishment, and what remains is social enforcement, kin pressure, and incumbent gatekeeping rather than law. Theater is 0.42 and rising slowly: the rites remain functional, but the articulated rationale for the partition has thinned to habit and inherited formula, with almost no participant able to state why the domains must not merge. Accessibility collapse is 0.40, since alternatives demonstrably persist (nonreligious funerals, secular graves, small-scale shrine funerals) but carry real friction. Resistance is 0.28: the great resistance wave was broken in the 1870s, and contemporary posture is closer to indifference than opposition. The temporal series run on one shared ten-point grid so every tracked metric is authored at every examined time point; the 1946 discontinuity (points 70 to 78) is the interval's structural event, an enforcement collapse that cut suppression by two-thirds and extractiveness nearly in half while leaving the partitioned practice itself intact.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the agenda-setter seat the partition was a design task completed and abandoned; nothing in that seat's situation registers the ongoing fee flows. From the temple-establishment seat the arrangement is livelihood and identity: a jurisdiction it suffered to win and now administers, which reads as legitimate stewardship of the dead. From the danka-household seat the same structure arrives as inherited obligation, fees detached from felt belief. From the fused-community and jinguji seats the arrangement is a wound: sites split, lineages erased. The engine derives these divergent per-seat types from power, exit, and role data; the divergence between the temple seat and the household seat is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (temple establishment, shrine priesthood, Meiji ideologues) derive directionality near the beneficiary end; the ideologues sit nearest it because their exit was total arbitrage, they collected legitimation and left. Danka households derive near the target end, damped by their declared secondary benefit, they do receive the services they pay for. Fused village communities and jinguji institutions derive at the full-target end: trapped, no exit, and the enforcement fell directly on them. The excluded advocates register the foreclosure of an alternative rather than a payment flow. National spatial scope amplifies effective extraction modestly for the target seats, per the engine's scope scaling; suppression stays unscaled as a raw structural property.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical mislabels. Reading the arrangement as pure extraction ignores that the allocation function is real and broadly endorsed: the born-Shinto-die-Buddhist settlement is a division of labor households largely affirm, and the purity stratum answers a problem that predates and survives the state. Reading it as pure coordination ignores its installation history and rent structure: the modern partition was imposed by coercion, retains hereditary fee lock-in, and forecloses the fused practice it displaced. On mandatrophy: the Meiji mandate, a sovereign national cult, died at disestablishment and no beneficiary openly defends it; the older purity-management stratum persists in attenuated form; hence founding_problem_status contested. A consumer reading the founding narrative as self-serving flag should find corroboration for exactly that: the nationalist stratum is dead, the practical stratum lives, and the arrangement's persistence is carried by habit and revenue rather than by either mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_index,
    'This story authors only the domain_partition reading of the kami_buddha_ontology kernel; would instantiating the honji_suijaku_monism or incoherent_bundle readings yield a different beneficiary structure and epsilon for the same colloquial label?',
    'Generate the sibling stories and compare computed classifications; convergence across readings would suggest the label tracks one underlying structure, systematic divergence confirms the decomposition into a constraint family.',
    'If the monism reading computes with a different victim set (doctrinal elites extracting interpretive authority rather than households paying fees), the corpus must treat shinbutsu-shugo as a family of linked stories, never as one constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_index, conceptual, 'Reading-index dependence of the kami-buddha kernel''s classification.').

omega_variable(
    enforcement_epoch_referent,
    'Does the end-state epsilon measure the Meiji-enforced partition or the post-disestablishment customary settlement, and can a single scalar honestly span the 1946 discontinuity?',
    'Compare household payments for equivalent mortuary services in districts where shrine-temple fusion survived longest against fully separated districts; differential pricing isolates the enforcement legacy.',
    'If survivor-district households pay systematically less, the partition premium is an enforcement artifact and the end-state extractiveness is overstated; if pricing converges, the contemporary arrangement is closer to priced service.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_epoch_referent, empirical, 'Which epoch''s arrangement the terminal epsilon describes.').

omega_variable(
    danka_payment_voluntariness,
    'Are contemporary household payments to temples purchases of valued services or obligation-driven transfers that would largely cease under free choice?',
    'Revealed-preference tracking: adoption rates of nonreligious funerals, family-grave dissolution applications, and the survey gap between stated belief and continued payment.',
    'A wide stated-belief-versus-payment gap implies the transfer rides on social enforcement, raising the payer seat''s effective extraction; a narrow gap supports the service-cost framing and lowers it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(danka_payment_voluntariness, empirical, 'Voluntary-purchase versus obligation character of danka transfers.').

omega_variable(
    purity_stratum_nativeness,
    'Is the life/death domain split a native structure of kami worship or an artifact of Buddhist defilement doctrine absorbed into Shinto self-understanding?',
    'Philological and archaeological work on pre-Buddhist mortuary practice at sacred sites: burial proximity to shrine precincts, earliest attested death taboos, Kojiki-era purity regulations.',
    'If native, the partition''s coordination stratum is deep and durable and survives even full doctrinal re-fusion; if imported, it is a contingent overlay whose removal would cost little functional capacity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(purity_stratum_nativeness, conceptual, 'Origin ambiguity of the purity/death jurisdictional division.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__domain_partition, 0, 155).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__domain_partition, theater_ratio, 0, 0.16).
narrative_ontology:measurement(kami_tr_t3, kami_buddha_ontology__domain_partition, theater_ratio, 3, 0.08).
narrative_ontology:measurement(kami_tr_t10, kami_buddha_ontology__domain_partition, theater_ratio, 10, 0.13).
narrative_ontology:measurement(kami_tr_t25, kami_buddha_ontology__domain_partition, theater_ratio, 25, 0.19).
narrative_ontology:measurement(kami_tr_t45, kami_buddha_ontology__domain_partition, theater_ratio, 45, 0.23).
narrative_ontology:measurement(kami_tr_t70, kami_buddha_ontology__domain_partition, theater_ratio, 70, 0.29).
narrative_ontology:measurement(kami_tr_t78, kami_buddha_ontology__domain_partition, theater_ratio, 78, 0.31).
narrative_ontology:measurement(kami_tr_t100, kami_buddha_ontology__domain_partition, theater_ratio, 100, 0.35).
narrative_ontology:measurement(kami_tr_t130, kami_buddha_ontology__domain_partition, theater_ratio, 130, 0.39).
narrative_ontology:measurement(kami_tr_t155, kami_buddha_ontology__domain_partition, theater_ratio, 155, 0.42).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__domain_partition, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(kami_be_t3, kami_buddha_ontology__domain_partition, base_extractiveness, 3, 0.88).
narrative_ontology:measurement(kami_be_t10, kami_buddha_ontology__domain_partition, base_extractiveness, 10, 0.82).
narrative_ontology:measurement(kami_be_t25, kami_buddha_ontology__domain_partition, base_extractiveness, 25, 0.74).
narrative_ontology:measurement(kami_be_t45, kami_buddha_ontology__domain_partition, base_extractiveness, 45, 0.66).
narrative_ontology:measurement(kami_be_t70, kami_buddha_ontology__domain_partition, base_extractiveness, 70, 0.7).
narrative_ontology:measurement(kami_be_t78, kami_buddha_ontology__domain_partition, base_extractiveness, 78, 0.36).
narrative_ontology:measurement(kami_be_t100, kami_buddha_ontology__domain_partition, base_extractiveness, 100, 0.43).
narrative_ontology:measurement(kami_be_t130, kami_buddha_ontology__domain_partition, base_extractiveness, 130, 0.48).
narrative_ontology:measurement(kami_be_t155, kami_buddha_ontology__domain_partition, base_extractiveness, 155, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__domain_partition, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(kami_su_t3, kami_buddha_ontology__domain_partition, suppression_requirement, 3, 0.9).
narrative_ontology:measurement(kami_su_t10, kami_buddha_ontology__domain_partition, suppression_requirement, 10, 0.84).
narrative_ontology:measurement(kami_su_t25, kami_buddha_ontology__domain_partition, suppression_requirement, 25, 0.74).
narrative_ontology:measurement(kami_su_t45, kami_buddha_ontology__domain_partition, suppression_requirement, 45, 0.66).
narrative_ontology:measurement(kami_su_t70, kami_buddha_ontology__domain_partition, suppression_requirement, 70, 0.7).
narrative_ontology:measurement(kami_su_t78, kami_buddha_ontology__domain_partition, suppression_requirement, 78, 0.2).
narrative_ontology:measurement(kami_su_t100, kami_buddha_ontology__domain_partition, suppression_requirement, 100, 0.24).
narrative_ontology:measurement(kami_su_t130, kami_buddha_ontology__domain_partition, suppression_requirement, 130, 0.29).
narrative_ontology:measurement(kami_su_t155, kami_buddha_ontology__domain_partition, suppression_requirement, 155, 0.34).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__domain_partition, resource_allocation).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, incoherent_bundle).

% DUAL FORMULATION NOTE:
% The colloquial label shinbutsu-shugo conflates at least three structurally distinct claims, decomposed per the epsilon-invariance principle into a linked family. This story authors the domain-partition reading: two parallel ontologies with enforced separation, epsilon near 0.5 mixing genuine coordination with monopoly rents. The honji-suijaku monism reading (hierarchical identity doctrine) has a different beneficiary structure, doctrinal elites collecting interpretive authority rather than households paying fees, and therefore its own epsilon and classification. The incoherent_bundle reading is a meta-claim denying stable classification for the aggregate. Upstream-downstream: the partition reading's enforcement created the institutional conditions (destroyed intermediaries, hardened jurisdictions) against which the monism reading's revival pressure and the bundle reading's diagnosis operate. Each member links to the others here; no member should absorb another's epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
