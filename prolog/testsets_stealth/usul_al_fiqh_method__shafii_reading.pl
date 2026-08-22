% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__shafii_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__shafii_reading
 *   human_readable: Shafi'i Source-Hierarchy Regime: Authentication-Gated Legal Derivation
 *   domain: religious/legal/methodological
 *
 * SUMMARY:
 *   The Shafi'i methodological regime makes verified transmission the gateway
 *   to legal knowledge: no report grounds a ruling until its chain of
 *   transmitters survives criticism, analogy operates only where verified
 *   texts fall silent, consensus binds only in the Companions' generation,
 *   and the whole ordering is taught as a formal meta-discipline that ranks
 *   the sources before any derivation begins. The regime solved a real
 *   collective-action problem — post-prophetic law was fragmenting into
 *   whatever each region's jurists preferred, amid documented report
 *   fabrication — and it simultaneously rerouted derivational authority
 *   toward the specialists who run the verification apparatus and away from
 *   jurists whose standing rested on reasoned opinion and local practice.
 *   This file is ONE reading of the contested kernel usul_al_fiqh_method; per
 *   the epsilon-invariance principle the four readings are separate
 *   constraints with separate epsilon values, linked through the network
 *   block, and the inter-reading contest is routed to the omega variables
 *   rather than folded into this classification. The claim and the metrics
 *   are independent authored facts: the tangled_rope claim states what this
 *   seat believes is structurally true (genuine coordination plus asymmetric
 *   gatekeeping), and the metric values state what is descriptively true of
 *   the regime's operation; the engine computes per-seat types from the
 *   structural data.
 *
 * KEY AGENTS:
 *   - - hadith_transmission_specialists: Primary beneficiary (organized/identity_locked) — their authentication verdicts are the toll every derivation must pass
 *   - - shafii_methodology_establishment: Agenda setter (institutional/identity_locked) — teaches, polices, and reproduces the source hierarchy across generations
 *   - - rationalist_method_jurists: Primary target (powerful/constrained) — bear the loss of derivational authority their methods once carried
 *   - - uncredentialed_local_jurists: Secondary target (powerless/trapped) — practice-based local authority does not survive the credential gate
 *   - - aspiring_jurists: Cost-bearing entrants (powerless/constrained) — pay years of transmission training before licensed derivation
 *   - - muslim_lay_communities: Dual-positioned (organized/constrained) — receive predictable, uniform law and bear its rigidity
 *   - - qadi_judiciary: Enforcing administrators (institutional/constrained) — apply the hierarchy in court while surrendering personal discretion
 *   - - comparative_law_historians: Analytical observer (analytical/analytical) — sees the full four-reading structure from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, 0.46).
domain_priors:suppression_score(usul_al_fiqh_method__shafii_reading, 0.58).
domain_priors:theater_ratio(usul_al_fiqh_method__shafii_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__shafii_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__shafii_reading, "Shafi'i Source-Hierarchy Regime: Authentication-Gated Legal Derivation").
narrative_ontology:topic_domain(usul_al_fiqh_method__shafii_reading, "religious/legal/methodological").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__shafii_reading, '8cd4fc35-c468-4867-b482-d2a724e1a58b').
narrative_ontology:cs_kernel_codification('8cd4fc35-c468-4867-b482-d2a724e1a58b', formalized).
narrative_ontology:cs_authority_grounding('8cd4fc35-c468-4867-b482-d2a724e1a58b', lineage).
narrative_ontology:cs_interpretation_layer_present('8cd4fc35-c468-4867-b482-d2a724e1a58b').
narrative_ontology:cs_reading_relation('8cd4fc35-c468-4867-b482-d2a724e1a58b', usul_al_fiqh_method__hanafi_reading, forecloses).
narrative_ontology:cs_reading_relation('8cd4fc35-c468-4867-b482-d2a724e1a58b', usul_al_fiqh_method__maliki_reading, forecloses).
narrative_ontology:cs_reading_relation('8cd4fc35-c468-4867-b482-d2a724e1a58b', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('8cd4fc35-c468-4867-b482-d2a724e1a58b', foundational, authenticated_sunna_derivation_prerequisite).
narrative_ontology:cs_axiom_status(authenticated_sunna_derivation_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('8cd4fc35-c468-4867-b482-d2a724e1a58b', authenticated_sunna_derivation_prerequisite, theological).
narrative_ontology:cs_axiom('8cd4fc35-c468-4867-b482-d2a724e1a58b', foundational, qiyas_confined_to_textual_silence).
narrative_ontology:cs_axiom_status(qiyas_confined_to_textual_silence, holdable).
narrative_ontology:cs_axiom_grounding('8cd4fc35-c468-4867-b482-d2a724e1a58b', qiyas_confined_to_textual_silence, instrumental).
narrative_ontology:cs_axiom('8cd4fc35-c468-4867-b482-d2a724e1a58b', secondary, istihsan_categorically_invalid).
narrative_ontology:cs_axiom_status(istihsan_categorically_invalid, holdable).
narrative_ontology:cs_axiom_grounding('8cd4fc35-c468-4867-b482-d2a724e1a58b', istihsan_categorically_invalid, deontological).
narrative_ontology:cs_reference_frame('8cd4fc35-c468-4867-b482-d2a724e1a58b', prophetic_sunna_primacy_framework).
narrative_ontology:cs_drift_state('8cd4fc35-c468-4867-b482-d2a724e1a58b', modern_statutory_codification_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('8cd4fc35-c468-4867-b482-d2a724e1a58b', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, shafii_textualist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, rationalist_method_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, uncredentialed_local_jurists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, shafii_methodology_establishment).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, muslim_lay_communities).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, aspiring_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, muslim_lay_communities).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, isnad_criticism_reliability).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, companions_consensus_authority).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, closed_source_hierarchy_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Memorize, grade, and transmit reports together with their chains of narrators; publish the verdicts that determine which reports count in court and which do not. Every jurist who needs a report cleared must come through their appraisal. Leaving the trade would discard a lifetime of chain knowledge that has no market outside it; their standing, students, and patronage flow from holding the appraisal office.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists, beneficiary,
    organized, generational, identity_locked, continental).

% Teach the ranked order of sources in the madrasas, examine candidates on it, write the manuals that define it, and staff the appointments that decide who may derive law. The school's continuity is bound up with the method being followed; revising the hierarchy would unmake the institution that carries it. Collects institutional permanence and the authority of certifying others.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, shafii_methodology_establishment, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, shafii_methodology_establishment, beneficiary).

% Jurists whose standing was built on reasoned opinion, discretionary adjustment of strict results, and mastery of local practice. Under the regime their tools are ruled out of order: issuing on personal judgment invites censure, and advancement requires recasting their work in the approved method. Absorbing the demotion, reftraining late in life, or retiring to advisory marginality are the available paths; leaving jurisprudence altogether forfeits their life's capital.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, rationalist_method_jurists, payer,
    powerful, biographical, constrained, continental).

% Town and district jurists whose authority came from knowing the community and its settled practice rather than from chain-certified learning. The credential gate prices them out: the decades of transmission study it demands are beyond their means, while abandoning local practice abandons the constituency that sustains them. They continue practicing under a cloud of diminished standing.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, uncredentialed_local_jurists, payer,
    powerless, biographical, trapped, regional).

% Students who must pass through years of transmission training and methodology examination before anyone will accept their legal determinations. The entry price shapes who enters the profession — those with time, travel means, and access to teachers — and delays independent practice by the length of the credential path. Choosing another vocation is possible but forfeits the calling.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, aspiring_jurists, payer,
    powerless, biographical, constrained, regional).

% Receive rulings that no longer swing with each judge's temperament: the same report tends to yield the same result across regions, and fabrication scandals are screened before reaching the courtroom. They also bear the cost side — a closed method adapts slowly to new conditions, and questions the method does not anticipate wait on the methodologists. Their recourse is petitioning scholars, not revising the method.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, muslim_lay_communities, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, muslim_lay_communities, payer).

% Judges appointed to apply the ranked sources in court: they enforce the hierarchy daily, rejecting improperly supported claims and grounding decisions in cleared reports. The same hierarchy removes the personal discretion their predecessors exercised — where the texts and approved analogy run out, they must wait on the methodologists rather than decide from judgment. Enforcement is their office; constraint is its price.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, qadi_judiciary, agenda_setter,
    institutional, biographical, constrained, continental).

% Study the formation of Islamic legal methodology from outside the tradition's confessional commitments: they document the forgery waves, the polemics between methodological camps, and the differing source-lists of the schools. They collect no ruling authority and bear none of its costs; their product is the record the other seats argue over.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, comparative_law_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__shafii_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared procedure for deriving law from revelation across a geographically dispersed community: criteria for accepting or rejecting reports about the Prophet's practice, a ranked fallback order for cases the texts do not address, and a common standard that judges and scholars in different cities can appeal to when they disagree.
% TRANSFER_FUNCTION: Moves derivational authority from jurists who reason from local practice and personal judgment toward specialists who verify transmission chains; moves the price of entry onto anyone seeking to issue legal determinations, payable in years of transmission study; moves interpretive disputes from local courts toward the teaching centers where the hierarchy is defined.
% ABSENT_VOICES: The jurists whose standing rested on reasoned opinion and regional practice would object that the gate confuses documentary pedigree with legal wisdom — they sit outside the methodology seminars, in the courts and circles of the rival camps. Lay communities bearing the rigidity of a closed method have no seat in methodological debate at all; nor do the women and non-elite Muslims whose access to legal knowledge ran through local jurists rather than chain-certified scholars.
% DISAPPEARANCE_RATIONALE: Courts would revert to divergent regional methods within a generation; report screening would lose its institutional home and fabrication scandals would recur unscreened; the map of juridical authority would redraw around whichever methods regaining jurists favored; transmission scholarship would persist as religious history but lose its jurisdiction over law.
% FOUNDING_PROBLEM: After the Prophet's death the community faced proliferating and mutually contradictory reports about his practice, documented fabrication of such reports during political strife, and judges in different provinces deciding identical cases on incompatible principles — law risked dissolving into whatever each governor's appointee preferred.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Hanafi and Maliki jurists, who reject the Shafi'i restrictions, nonetheless affirm in their own methodology manuals both the fabrication problem and the necessity of ranked sources; historians of early Islamic law working from non-confessional records document the forgery waves and adjudicative fragmentation independently; and the survival of transmission criticism in every school — including those the gate disadvantages — attests that the underlying problem was real rather than an artifact of one faction's interest.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__shafii_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__shafii_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__shafii_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__shafii_reading_tests).
:- end_tests(usul_al_fiqh_method__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.46 at interval end) because the regime genuinely transfers authority — the credential gate converts verification skill into jurisdiction over what counts as law — while the reading's own lights count much of the transfer as the price of epistemic discipline; the rising series models the gate consolidating from reform program into settled establishment. Suppression (0.58) is a raw structural property, unscaled by power or scope: it consists of credential monopoly, appointment norms favoring chain-certified scholars, and polemical exclusion of discretionary methods — social and institutional force, not physical coercion. Theater is low (0.18) because chain criticism performs real filtering work; the slow rise tracks ritualization at the margins (chain-length prestige outliving its discriminating function). The suppression_requirement series falls from 0.72 to 0.58: the story this traces is enforcement DECAY after victory — the formative era demanded intense polemical enforcement against the entrenched rationalist establishment, and as the hierarchy normalized, the machinery relaxed toward the stable residual the scalar records. Accessibility collapse sits at 0.42 because alternatives collapsed hard inside Shafi'i-governed institutions yet survived across the wider landscape in rival schools; resistance (0.62) reflects the documented polemical wars over discretionary method and Medinan practice. All three series run on one shared time grid (points 0-30, roughly mapping the formative-to-classical consolidation era, an authored abstraction stated here as an assumption) so every metric is asserted at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats should compute differently. From the transmission specialist's seat the regime is the protective discipline that keeps revelation distinguishable from invention — the gate is the point. From the rationalist jurist's seat the same gate is confiscation: a lifetime of demonstrated legal judgment is demoted beneath a chain-of-names credential, and the ban on discretionary method reads as the establishment monopolizing derivation. The qadi seat is squeezed from both sides — the hierarchy lends rulings legitimacy and uniformity while stripping the personal discretion that earlier judges exercised — so that seat should sit nearer symmetric than either pole. Lay communities sit near symmetric: uniform law is a real good, rigidity a real cost. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Transmission specialists are declared beneficiaries with identity-locked exit — their professional selves are constituted by the chain discipline — placing them near the subsidized end. The methodology establishment administers and enforces, collecting institutional permanence, likewise beneficiary-side. Rationalist jurists are declared victims with constrained exit: conversion to textualism means discarding accumulated method-capital, so they sit near the full-target end. Uncredentialed local jurists are victims with trapped exit — their constituency is local and their resources cannot fund the credential path — the highest effective extraction in the structure. Aspiring jurists pay the entry price as victims. Lay communities carry both beneficiary and payer declarations, landing near symmetric. Qadis enforce the arrangement (agenda-setting side) while personally bearing its discretion cost; the derivation reads their administrative role, and the commentary records the squeeze qualitatively rather than overriding, since no single power-atom override can separate them from the establishment they serve alongside.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — post-prophetic authority crisis: proliferating reports, documented fabrication, judges reasoning without common standards — remains live, so no mandatrophy is declared and the arrangement has not outlived its mandate. The tangled_rope classification is what prevents mislabeling in both directions: reading the regime as pure coordination erases the documented losers (the rationalist jurists whose authority the gate confiscated, the local jurists it stranded); reading it as pure extraction erases the real solved problem (fragmentation and fabrication) that every rival school also had to answer. The temporal series carries the drift watch: if theater_ratio climbs past 0.5 while the ijma restriction atrophies (see the companions_ijma_scope_atrophy omega), the regime would be sliding toward inertial performance — a former coordination kept alive by habit — and the classification should be revisited on that evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates the shafii_reading of kernel usul_al_fiqh_method; what structurally changes under the sibling readings (hanafi_reading, maliki_reading, hanbali_reading), and where exactly does the disagreement bite?',
    'Side-by-side comparison of the four madhhab usul corpora on three axes: source-list closure, authentication threshold, and ijma extension. The disagreement is located at source-closure and gatekeeping allocation, not at the existence of source-ranking itself.',
    'If a sibling reading instantiates the same coordination function with lower gatekeeping asymmetry, the measured extraction is attributable to the Shafi''i restriction set specifically rather than to methodological governance generally; this file''s classification must not be generalized to the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer frame: one reading of the usul al-fiqh kernel; sibling deltas and the locus of disagreement.').

omega_variable(
    isnad_authenticity_calibration,
    'Does the transmission-chain apparatus actually filter fabrication at the rate the credential gate''s authority presumes?',
    'Matn-isnad congruence studies and documented forgery-wave analysis (e.g., the post-civil-war fabrication epidemics the classical critics themselves catalogued), yielding error-rate estimates for the authentication filter.',
    'High authentication error rates would mean the gate transfers authority without delivering the epistemic good it charges for, raising the extraction share; low error rates would shift more of the measured epsilon into legitimate coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(isnad_authenticity_calibration, empirical, 'Whether the authentication gate tracks truth or merely tracks chain pedigree.').

omega_variable(
    gatekeeper_rent_netting,
    'Do transmission specialists net-gain from the gate, or does the discipline''s labor burden (decades of memorization and grading for modest livelihood) consume what the gatekeeping position yields?',
    'Economic-biography analysis of the muhaddith class: income sources, patronage flows, and career lengths compared to jurists of equivalent standing outside the authentication economy.',
    'If specialists net-lose materially, the beneficiary structure thins toward status rents only and the arrangement drifts toward pure coordination; if they net-gain, the asymmetric-extraction half of the structure is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeper_rent_netting, empirical, 'Whether the beneficiary seat captures net rents or symbolic compensation.').

omega_variable(
    companions_ijma_scope_atrophy,
    'Is the restriction of consensus to the Companions'' generation actually maintained, or has de facto admission of later scholarly consensus eroded the restriction without formal revision?',
    'Doctrinal analysis of how later Shafi''i authorities treated alleged post-Companion consensus claims (e.g., the concessions recorded in classical usul manuals when confronted with near-unanimous later practice).',
    'If later consensus is admitted in practice, the restriction atrophies, the exclusionary force declines, and the arrangement drifts toward a looser coordinative form; if held strictly, the gate''s closure persists and extraction remains concentrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(companions_ijma_scope_atrophy, conceptual, 'Whether the ijma restriction is live doctrine or honored in the breach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__shafii_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_shafii_tr_t0, usul_al_fiqh_method__shafii_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(usul_shafii_tr_t0, observed).
narrative_ontology:measurement(usul_shafii_tr_t6, usul_al_fiqh_method__shafii_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement_basis(usul_shafii_tr_t6, observed).
narrative_ontology:measurement(usul_shafii_tr_t12, usul_al_fiqh_method__shafii_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement_basis(usul_shafii_tr_t12, observed).
narrative_ontology:measurement(usul_shafii_tr_t18, usul_al_fiqh_method__shafii_reading, theater_ratio, 18, 0.15).
narrative_ontology:measurement_basis(usul_shafii_tr_t18, observed).
narrative_ontology:measurement(usul_shafii_tr_t24, usul_al_fiqh_method__shafii_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement_basis(usul_shafii_tr_t24, observed).
narrative_ontology:measurement(usul_shafii_tr_t30, usul_al_fiqh_method__shafii_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(usul_shafii_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(usul_shafii_be_t0, usul_al_fiqh_method__shafii_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(usul_shafii_be_t0, observed).
narrative_ontology:measurement(usul_shafii_be_t6, usul_al_fiqh_method__shafii_reading, base_extractiveness, 6, 0.34).
narrative_ontology:measurement_basis(usul_shafii_be_t6, observed).
narrative_ontology:measurement(usul_shafii_be_t12, usul_al_fiqh_method__shafii_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement_basis(usul_shafii_be_t12, observed).
narrative_ontology:measurement(usul_shafii_be_t18, usul_al_fiqh_method__shafii_reading, base_extractiveness, 18, 0.42).
narrative_ontology:measurement_basis(usul_shafii_be_t18, observed).
narrative_ontology:measurement(usul_shafii_be_t24, usul_al_fiqh_method__shafii_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement_basis(usul_shafii_be_t24, observed).
narrative_ontology:measurement(usul_shafii_be_t30, usul_al_fiqh_method__shafii_reading, base_extractiveness, 30, 0.46).
narrative_ontology:measurement_basis(usul_shafii_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(usul_shafii_su_t0, usul_al_fiqh_method__shafii_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(usul_shafii_su_t0, observed).
narrative_ontology:measurement(usul_shafii_su_t6, usul_al_fiqh_method__shafii_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement_basis(usul_shafii_su_t6, observed).
narrative_ontology:measurement(usul_shafii_su_t12, usul_al_fiqh_method__shafii_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement_basis(usul_shafii_su_t12, observed).
narrative_ontology:measurement(usul_shafii_su_t18, usul_al_fiqh_method__shafii_reading, suppression_requirement, 18, 0.61).
narrative_ontology:measurement_basis(usul_shafii_su_t18, observed).
narrative_ontology:measurement(usul_shafii_su_t24, usul_al_fiqh_method__shafii_reading, suppression_requirement, 24, 0.59).
narrative_ontology:measurement_basis(usul_shafii_su_t24, observed).
narrative_ontology:measurement(usul_shafii_su_t30, usul_al_fiqh_method__shafii_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(usul_shafii_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__shafii_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'usul al-fiqh' conflates four structurally distinct methodological regimes that share a kernel but differ in source-closure, authentication threshold, and consensus extension. Each reading is authored as its own story with its own epsilon, beneficiaries, and victims, per the epsilon-invariance principle; this file (shafii_reading) is the most closure-exhaustive variant, and its gatekeeping asymmetry is attributable to the restriction set, not to source-ranking as such. Family members link through affects_constraints; cross-family contamination analysis should compare epsilon across the four files rather than averaging them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
