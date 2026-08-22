% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__incoherent_bundle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__incoherent_bundle, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: kami_buddha_ontology__incoherent_bundle
 *   human_readable: Shinbutsu-shugo as Institutionally Sustained Incoherent Bundle
 *   domain: religious/philosophical/institutional
 *
 * SUMMARY:
 *   This story instantiates the 'incoherent bundle' reading of the
 *   shinbutsu-shugo kernel: the claim that the fusion of kami and buddha
 *   worship in premodern Japan was never a single coherent ontological
 *   position but an institutionally sustained bundle holding simultaneous,
 *   mutually incompatible commitments (fusion and separation, hierarchy and
 *   reciprocity, systematized and unsystematized) together across centuries
 *   because the bundle's practical and administrative utility to
 *   shrine-temple complexes and ritual lineages did not require, and was
 *   arguably harmed by, doctrinal resolution. The two sibling readings —
 *   honji-suijaku monism (kami as traces of a buddha ground) and domain
 *   partition (kami and buddhas as ontologically distinct, governing separate
 *   life/death domains) — are NOT alternative descriptions of the same
 *   settled fact from different angles; per the ε-invariance principle, they
 *   are different constraints with different beneficiary structures,
 *   different victim sets, and different classifications, generated as
 *   separate stories and linked here only by network reference. This story's
 *   ε is intrinsic to the incoherence-as-institutionally-maintained claim and
 *   is not averaged against the monist or partition readings.
 *
 * KEY AGENTS:
 *   - shrine_temple_administrative_complexes: institutional beneficiary that switches ontological frames as convenience dictates
 *   - ritual_specialist_lineages: beneficiary whose expertise value depends on unresolved doctrine
 *   - systematic_theologians: payer whose coherent systematizations are absorbed without adjudication
 *   - lay_practitioners_seeking_doctrinal_clarity: powerless payer bearing the cognitive cost of unresolved metaphysics
 *   - meiji_shinbutsu_bunri_reformers: excluded external challenger whose forced resolution was administrative, not doctrinal
 *   - comparative_religion_scholars: analytical observer documenting the incoherence without authority to resolve it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, 0.58).
domain_priors:suppression_score(kami_buddha_ontology__incoherent_bundle, 0.42).
domain_priors:theater_ratio(kami_buddha_ontology__incoherent_bundle, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, extractiveness, 0.58).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__incoherent_bundle, piton).
narrative_ontology:human_readable(kami_buddha_ontology__incoherent_bundle, "Shinbutsu-shugo as Institutionally Sustained Incoherent Bundle").
narrative_ontology:topic_domain(kami_buddha_ontology__incoherent_bundle, "religious/philosophical/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__incoherent_bundle, '8ae91d32-7a34-4e08-a8a1-4c1d0285fb48').
narrative_ontology:cs_kernel_codification('8ae91d32-7a34-4e08-a8a1-4c1d0285fb48', distributed).
narrative_ontology:cs_authority_grounding('8ae91d32-7a34-4e08-a8a1-4c1d0285fb48', practice).
narrative_ontology:cs_interpretation_layer_present('8ae91d32-7a34-4e08-a8a1-4c1d0285fb48').
narrative_ontology:cs_reading_relation('8ae91d32-7a34-4e08-a8a1-4c1d0285fb48', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('8ae91d32-7a34-4e08-a8a1-4c1d0285fb48', kami_buddha_ontology__domain_partition, coexists_with).
narrative_ontology:cs_axiom('8ae91d32-7a34-4e08-a8a1-4c1d0285fb48', foundational, no_single_ontology_required_for_ritual_efficacy).
narrative_ontology:cs_axiom_status(no_single_ontology_required_for_ritual_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('8ae91d32-7a34-4e08-a8a1-4c1d0285fb48', no_single_ontology_required_for_ritual_efficacy, conventional).
narrative_ontology:cs_axiom('8ae91d32-7a34-4e08-a8a1-4c1d0285fb48', foundational, institutional_practice_precedes_and_survives_doctrinal_settlement).
narrative_ontology:cs_axiom_status(institutional_practice_precedes_and_survives_doctrinal_settlement, holdable).
narrative_ontology:cs_axiom_grounding('8ae91d32-7a34-4e08-a8a1-4c1d0285fb48', institutional_practice_precedes_and_survives_doctrinal_settlement, empirically_contingent).
narrative_ontology:cs_reference_frame('8ae91d32-7a34-4e08-a8a1-4c1d0285fb48', pre_heian_accommodation_arrangement).
narrative_ontology:cs_drift_state('8ae91d32-7a34-4e08-a8a1-4c1d0285fb48', meiji_and_post_meiji_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8ae91d32-7a34-4e08-a8a1-4c1d0285fb48', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, shrine_temple_administrative_complexes).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, ritual_specialist_lineages).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, systematic_theologians).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, lay_practitioners_seeking_doctrinal_clarity).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__incoherent_bundle, practical_efficacy_over_theoretical_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jointly administered shrine-temple complexes (jingu-ji) draw revenue and legitimacy from operating both kami rites and buddhist rites on the same grounds, for the same patrons, without ever resolving whether the kami and the buddha enshrined there are the same being, complementary beings, or unrelated beings assigned to different life-events. They can invoke fusion language when it serves fundraising or imperial legitimation and invoke separation language when it serves jurisdictional claims against a rival institution, switching frames as convenience dictates.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, shrine_temple_administrative_complexes, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, shrine_temple_administrative_complexes, agenda_setter).

% Hereditary priestly and monastic lineages hold transmitted ritual knowledge whose value depends on the bundle staying unresolved: a settled ontology would make their mediating expertise replaceable by doctrine anyone could read. Their livelihoods and social standing are built on performing rites whose coherence is never asked of them, only their correct execution.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, ritual_specialist_lineages, beneficiary,
    organized, generational, constrained, regional).

% Scholar-monks and Confucian-influenced systematizers across the centuries who attempted to render shinbutsu-shugo into a single coherent doctrine (whether honji-suijaku monism or strict domain partition) repeatedly found their systematizations locally adopted, then locally contradicted by a neighboring institution's competing systematization, then absorbed back into practice as one more ritual option among several. Their intellectual labor is structurally unable to bind the institutions that host the practice.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, systematic_theologians, payer,
    moderate, biographical, constrained, national).

% Ordinary worshippers who want to know what they are actually venerating, and what it means for their salvation, mourning, or purification obligations, receive whichever answer the local institution finds administratively convenient that season. They bear the cognitive and existential cost of an unresolved metaphysics without any venue to demand resolution; the bundle's incoherence is invisible to them because it is never presented as incoherence, only as tradition.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, lay_practitioners_seeking_doctrinal_clarity, payer,
    powerless, biographical, trapped, local).

% State reformers who forcibly separated kami and buddha worship in 1868 tried to impose a clean domain-partition ontology by administrative fiat, dismantling jingu-ji complexes. Their attempt is excluded from the bundle's own self-understanding as an external imposition rather than a resolution generated from within the tradition; the persistence of syncretic practice after the reform, and its partial re-fusion in later decades, is evidence the underlying bundle was never actually resolved by the decree, only administratively suppressed for a period.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, meiji_shinbutsu_bunri_reformers, excluded,
    institutional, generational, constrained, national).

% Historians and religious studies scholars documenting shinbutsu-shugo across centuries observe that no single ontology description fits all the textual, ritual, and institutional evidence simultaneously — sources support fusion, separation, hierarchy, and reciprocity depending on which text, site, and period is sampled. They can name the incoherence but hold no authority to resolve or discipline the institutions that sustain it.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The bundle coordinates religious, political, and administrative functions across a plural landscape of shrines, temples, courts, and lineages that would otherwise compete or conflict over jurisdiction, legitimacy, and ritual authority — by never requiring any single ontology to be settled, each institution can claim whichever framing (fusion, separation, hierarchy) advances its position in a given dispute without having to defeat rival institutions on doctrinal grounds.
% TRANSFER_FUNCTION: Moves interpretive authority, ritual fees, land grants, and imperial patronage toward whichever institutional complex can most flexibly redeploy the fusion/separation/hierarchy vocabulary to fit the occasion, and moves the cost of unresolved meaning onto lay practitioners and onto systematizing scholars whose coherent accounts are absorbed, ignored, or locally overridden rather than adjudicated.
% ABSENT_VOICES: Lay practitioners across the centuries who wanted a stable answer to 'what am I actually venerating' were never given a forum to demand doctrinal settlement; systematic theologians who did press for coherence were folded into practice as one optional strand rather than answered on their own terms. Meiji-era separation reformers pressed the sharpest external challenge and were administratively excluded from ever being tested against the tradition's own logic — the decree bypassed the incoherence rather than resolving it.
% DISAPPEARANCE_RATIONALE: Institutions and ritual specialists would insist the bundle's disappearance (i.e., forced resolution into one coherent ontology) would rearrange centuries of jointly-administered sites, land arrangements, and lineage transmission — much as the Meiji separation edict in fact did rearrange institutional geography. Comparative scholars note that lay religious practice at the site level continued largely unaffected by ontological resolution one way or the other, since most worshippers engage ritually rather than doctrinally; whether the 'world' that depends on the bundle is the institutional world or the devotional world is exactly the contested point.
% FOUNDING_PROBLEM: Early Japanese Buddhism needed to establish legitimacy and coexist with entrenched local kami cults rather than displace them outright; some working relationship between imported buddhas and indigenous kami was needed to avoid a costly, possibly unwinnable contest for ritual territory.
% FOUNDING_PROBLEM_CORROBORATION: Shrine-temple administrators and ritual lineages attest the accommodation problem remains live (coexistence still requires practical management). Comparative religion scholars and the historical record of the Meiji separation attest that the original coexistence problem was substantially resolved centuries ago and what persists now is institutional and ritual-economic inertia dressed as unbroken tradition — the bundle's continued incoherence serves administrative flexibility more than it serves any live theological need.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__incoherent_bundle, contested).
narrative_ontology:founding_problem_status(kami_buddha_ontology__incoherent_bundle, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__incoherent_bundle, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kami_buddha_ontology__incoherent_bundle, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__incoherent_bundle, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__incoherent_bundle_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__incoherent_bundle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) rather than severe: the bundle does not violently dispossess anyone, but it does structurally transfer interpretive authority and ritual revenue toward institutions that benefit from ambiguity, at the diffuse cost of practitioners and systematizers who never get resolution. Theater ratio is authored as the dominant signal (0.71) because the piton reading turns centrally on this: what looks like sustained theological engagement across centuries is, on this reading, increasingly the administration of practical ritual efficacy dressed in the vocabulary of theological continuity — the doctrine is invoked, not adjudicated. Accessibility collapse is moderate-low (0.4): unlike a mountain, alternative framings (partition, monism, outright rejection) remain visible and periodically attempted (Meiji separation is the clearest instance), but none succeed in permanently displacing the bundle. Resistance is moderate (0.55): systematic theologians and reform movements do actively contest the incoherence, but their resistance is repeatedly absorbed rather than defeated.
 *
 * PERSPECTIVAL GAP:
 *   From the shrine-temple administrative seat, the bundle looks like living tradition — flexible, resilient, serving its communities across thirteen centuries. From the systematic theologian's seat and the lay practitioner's seat, the same structure looks like an institution that has never had to answer its own central question, extracting compliance, revenue, and reverence without ever being obligated to state coherently what is being revered. The engine should register this as the classic piton signature: a former (or perpetually deferred) resolution problem, sustained now mostly by institutional inertia and ritual competence rather than by any live coordination or extraction imperative — no single seat profits enough to be a snare's clear beneficiary; the shrine-temple complexes and ritual lineages benefit from ambiguity but do not extract acutely, they simply are not required to resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   Shrine-temple complexes and ritual lineages sit near the beneficiary end: they retain institutional flexibility and expert-mediation value precisely because the ontology stays unsettled, and their exit options (arbitrage, constrained) reflect real capacity to redeploy the ambiguity to their advantage. Systematic theologians and lay practitioners sit toward the target end: they bear the unresolved cost — intellectually for the theologians, existentially for the laity — and their exit options (constrained, trapped) reflect limited capacity to force resolution. The Meiji reformers are a special case: institutionally powerful but structurally excluded from the bundle's self-understanding, which is why they are marked excluded rather than payer or beneficiary — their intervention is registered by the tradition as an assault from outside, not a resolution generated from within.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (early Buddhist-kami coexistence to avoid ritual-territorial conflict) is authored as contested rather than flatly dead, because shrine-temple complexes can truthfully say some coexistence management is still functionally needed today. But the founding_problem_status/disappearance_verdict pairing (contested/contested) is the diagnostic the engine should read: if the coexistence problem the bundle was built to solve was substantially resolved by the medieval period, and what persists since is institutional and economic inertia dressed as continuous tradition, that is a capture/zombie signature distinct from a genuine, still-live coordination need. Classifying this reading as piton (rather than tangled_rope or rope) reflects the judgment that no single seat is extracting acutely enough to be a snare's beneficiary — the shrine-temple complexes profit from flexibility, not from a concentrated rent — while the theoretical incoherence persists mostly because dismantling it costs the administering institutions more (in land, lineage disruption, ritual continuity) than any party is willing to pay, exactly the piton cost-asymmetry test.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incoherence_vs_undertheorized_pluralism,
    'Is the coexistence of fusion, separation, hierarchy, and reciprocity framings genuine logical incoherence, or is it better described as an undertheorized but locally consistent pluralism — different sites and periods each internally coherent, with the appearance of contradiction arising only when a scholar aggregates across sites that were never meant to be read together?',
    'Fine-grained site-by-site and period-by-period textual analysis testing whether any single institution or lineage held mutually contradictory commitments simultaneously, versus holding one consistent local framing that simply differed from a neighboring institution''s framing.',
    'If local consistency holds and only cross-site aggregation produces the appearance of contradiction, the ''incoherent bundle'' classification may overstate incoherence that is actually institutional diversity — pushing the reading closer to a rope (genuine plural coordination, no single victim of incoherence) rather than a piton sustained by masked contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherence_vs_undertheorized_pluralism, conceptual, 'Whether the bundle is genuinely self-contradictory or merely unaggregated local pluralism.').

omega_variable(
    meiji_separation_as_resolution_or_suppression,
    'Did the 1868 Meiji shinbutsu bunri edict actually resolve the underlying ontological incoherence by force, or did it merely suppress the syncretic practice administratively while leaving the underlying bundle''s logical structure untouched — evidenced by partial re-fusion and continued syncretic practice at the local level after the reform receded?',
    'Historical tracking of post-Meiji shrine-temple practice: degree and persistence of re-syncretism versus durable separation across regions and decades.',
    'If separation proved durable and complete, the founding problem may be better read as dead and formally resolved by state action (supporting a mandatrophy verdict of resolved-then-defunct persistence elsewhere); if re-fusion was substantial, it supports this reading''s claim that the bundle''s incoherence survives any single institutional attempt to resolve it, reinforcing the piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_separation_as_resolution_or_suppression, empirical, 'Whether administrative separation actually resolved the ontological bundle or only suppressed it temporarily.').

omega_variable(
    committer_frame_which_reading_is_the_default,
    'Given that all three kernel readings (domain_partition, honji_suijaku_monism, incoherent_bundle) have textual support across different sources and periods, is there a principled basis for treating this incoherent-bundle reading as the historically dominant or most descriptively accurate account of the kernel as a whole, rather than as one more reading whose own claim to comprehensiveness (''there is no single ontology'') is itself contestable by parties committed to one of the other two readings?',
    'None fully available within the historical record alone; would require a meta-level methodological argument about how to weight textual plurality versus institutional practice as evidence for ontological commitment, which is itself a matter of scholarly framing rather than settled fact.',
    'If a principled basis for comprehensiveness exists, this reading''s claim to describe the whole tradition (rather than being merely one contested strand within it) strengthens; if not, this reading''s ε and classification should be understood as itself one contested position among the three, on exactly the same epistemic footing as the monism and partition readings it is being generated alongside.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_which_reading_is_the_default, conceptual, 'Whether the incoherent-bundle reading''s claim to describe the whole kernel is itself just one more contested reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__incoherent_bundle, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__incoherent_bundle, theater_ratio, 0, 0.25).
narrative_ontology:measurement(kami_tr_t200, kami_buddha_ontology__incoherent_bundle, theater_ratio, 200, 0.38).
narrative_ontology:measurement(kami_tr_t400, kami_buddha_ontology__incoherent_bundle, theater_ratio, 400, 0.5).
narrative_ontology:measurement(kami_tr_t600, kami_buddha_ontology__incoherent_bundle, theater_ratio, 600, 0.6).
narrative_ontology:measurement(kami_tr_t800, kami_buddha_ontology__incoherent_bundle, theater_ratio, 800, 0.68).
narrative_ontology:measurement(kami_tr_t1000, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1000, 0.71).
narrative_ontology:measurement(kami_tr_t1200, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1200, 0.71).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(kami_be_t200, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 200, 0.42).
narrative_ontology:measurement(kami_be_t400, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 400, 0.48).
narrative_ontology:measurement(kami_be_t600, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 600, 0.5).
narrative_ontology:measurement(kami_be_t800, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 800, 0.53).
narrative_ontology:measurement(kami_be_t1000, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1000, 0.56).
narrative_ontology:measurement(kami_be_t1200, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1200, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kami_buddha_ontology__incoherent_bundle, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__incoherent_bundle, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, domain_partition).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the kami_buddha_ontology kernel. honji_suijaku_monism claims ontological identity (kami as traces of a buddha ground) with correspondingly low extraction and a coordination-heavy classification where the hierarchy is doctrinally justified rather than administratively convenient. domain_partition claims ontological distinctness with functional separation, with its own distinct beneficiary/victim structure organized around jurisdictional boundaries (life-rites vs. death-rites institutions). This incoherent_bundle reading claims neither monism nor partition was ever the tradition's actual settled commitment, and authors correspondingly higher theater_ratio and a piton classification reflecting sustained institutional inertia rather than either doctrinal coordination or authority extraction. Each story carries its own independent ε; they are not to be averaged or reconciled, per the ε-invariance principle — the sibling files should be consulted for how their metrics diverge from this one's.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
