% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__maliki_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__maliki_reading
 *   human_readable: Maliki Usul al-Fiqh: Medinan Practice, Maslaha Mursala, and 'Urf as Independent Sources
 *   domain: Islamic Jurisprudence / Legal Theory / Comparative Law
 *
 * SUMMARY:
 *   This story instantiates the Maliki reading of the usul_al_fiqh_method
 *   kernel: the claim that the continuous practice of the Medinan community
 *   ('amal ahl al-Madina) carries independent evidentiary weight alongside
 *   hadith, that maslaha mursala (unrestricted public interest) is a valid
 *   legal source, and that regional custom ('urf) is integrated wherever it
 *   does not contradict clear text. This is a distinct constraint from the
 *   Hanafi, Shafi'i, and Hanbali readings of the same kernel — each of those
 *   readings resolves the same underlying problem (how to derive law where
 *   text is silent or ambiguous) with a structurally different source
 *   hierarchy, producing different beneficiary/victim configurations and
 *   different ε profiles. This file does not attempt to average across those
 *   readings or describe their contest; it characterizes only the Maliki
 *   reading on its own terms.
 *
 * KEY AGENTS:
 *   - medinan_juristic_lineage: Primary agenda-setter and beneficiary — administers what counts as 'amal and adjudicates via maslaha mursala
 *   - local_customary_authorities: Beneficiary — regional custom gains legal standing without full textual proof-texting
 *   - regional_maliki_qadis: Beneficiary/agenda-setter — expanded judicial discretion
 *   - non_medinan_legal_communities: Payer — bear a source hierarchy privileging one city's historical custom
 *   - textualist_minority_dissenters: Payer/excluded — object to precedent overriding authenticated hadith
 *   - comparative_jurists: Analytical observer — traces divergence across the four schools
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, 0.38).
domain_priors:suppression_score(usul_al_fiqh_method__maliki_reading, 0.32).
domain_priors:theater_ratio(usul_al_fiqh_method__maliki_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__maliki_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__maliki_reading, "Maliki Usul al-Fiqh: Medinan Practice, Maslaha Mursala, and 'Urf as Independent Sources").
narrative_ontology:topic_domain(usul_al_fiqh_method__maliki_reading, "Islamic Jurisprudence / Legal Theory / Comparative Law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__maliki_reading, '119badb4-a47b-4e65-8f3a-1ec07cbe7d87').
narrative_ontology:cs_kernel_codification('119badb4-a47b-4e65-8f3a-1ec07cbe7d87', distributed).
narrative_ontology:cs_authority_grounding('119badb4-a47b-4e65-8f3a-1ec07cbe7d87', lineage).
narrative_ontology:cs_interpretation_layer_present('119badb4-a47b-4e65-8f3a-1ec07cbe7d87').
narrative_ontology:cs_reading_relation('119badb4-a47b-4e65-8f3a-1ec07cbe7d87', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('119badb4-a47b-4e65-8f3a-1ec07cbe7d87', usul_al_fiqh_method__shafii_reading, influences).
narrative_ontology:cs_reading_relation('119badb4-a47b-4e65-8f3a-1ec07cbe7d87', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('119badb4-a47b-4e65-8f3a-1ec07cbe7d87', foundational, communal_practice_as_independent_evidentiary_source).
narrative_ontology:cs_axiom_status(communal_practice_as_independent_evidentiary_source, holdable).
narrative_ontology:cs_axiom_grounding('119badb4-a47b-4e65-8f3a-1ec07cbe7d87', communal_practice_as_independent_evidentiary_source, conventional).
narrative_ontology:cs_axiom('119badb4-a47b-4e65-8f3a-1ec07cbe7d87', foundational, unrestricted_public_interest_admissible_absent_text).
narrative_ontology:cs_axiom_status(unrestricted_public_interest_admissible_absent_text, holdable).
narrative_ontology:cs_axiom_grounding('119badb4-a47b-4e65-8f3a-1ec07cbe7d87', unrestricted_public_interest_admissible_absent_text, instrumental).
narrative_ontology:cs_reference_frame('119badb4-a47b-4e65-8f3a-1ec07cbe7d87', medinan_communal_transmission_as_living_sunna).
narrative_ontology:cs_drift_state('119badb4-a47b-4e65-8f3a-1ec07cbe7d87', post_classical_codification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('119badb4-a47b-4e65-8f3a-1ec07cbe7d87', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, medinan_juristic_lineage).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, local_customary_authorities).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, regional_maliki_qadis).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, non_medinan_legal_communities).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, textualist_minority_dissenters).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, practice_of_the_first_community_as_living_transmission).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, public_interest_as_legitimate_independent_source).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Transmits and administers the doctrine that the collective practice of Medina's inhabitants constitutes independent evidentiary weight equal to or exceeding solitary hadith reports, on the premise that practice handed down continuously in the Prophet's own city could not have departed from his teaching unnoticed. This lineage sets which customary practices qualify as 'amal and adjudicates disputes using maslaha mursala when text is silent, giving it durable interpretive authority that competing schools' methods do not grant to any single regional community.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, medinan_juristic_lineage, agenda_setter,
    institutional, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__maliki_reading, medinan_juristic_lineage, beneficiary).

% Merchants, guild elders, and tribal custom-holders across Maliki jurisdictions (North Africa, Andalusia, West Africa) whose local 'urf gets integrated into legal rulings so long as it does not contradict text. Their accumulated commercial and social practices become law-relevant without needing textual proof-texting for every instance, which is a direct structural benefit unavailable under more textually restrictive methods.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, local_customary_authorities, beneficiary,
    organized, generational, constrained, regional).

% Judges applying Maliki method invoke maslaha mursala and 'amal ahl al-Madina to reach flexible, locally-responsive rulings, which expands their discretionary authority relative to judges bound strictly to text-and-analogy chains. This discretion is professionally valuable to them but also binds their legal reasoning to continued deference to Medinan precedent even where local conditions have diverged sharply from eighth-century Hijaz.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, regional_maliki_qadis, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__maliki_reading, regional_maliki_qadis, agenda_setter).

% Muslim communities living far from Medina, under Maliki jurisdiction, whose own regional practices must yield priority to a semi-privileged status accorded to one city's historical custom, even when their own transmitted practice is comparably ancient or better attested. They bear the cost of a source hierarchy that treats one location as evidentially special without them having any voice in what counted as valid Medinan practice at the founding moment.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, non_medinan_legal_communities, payer,
    moderate, generational, constrained, continental).

% Individuals within Maliki jurisdictions who hold to a stricter textualist reading (closer to the Hanbali or Zahiri instinct) and object that elevating a city's aggregate custom to independent-source status risks displacing an authenticated hadith that conflicts with prevailing Medinan practice. They have limited standing to contest established Maliki rulings once codified, and where custom has hardened into precedent their objection carries no forum.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, textualist_minority_dissenters, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__maliki_reading, textualist_minority_dissenters, excluded).

% Scholars of comparative usul al-fiqh who study how the four Sunni schools diverge on source hierarchy, tracing how the Maliki elevation of 'amal and maslaha mursala produced different substantive rulings than Hanafi qiyas-expansion, Shafi'i hadith-primacy, or Hanbali text-restriction on the same underlying questions.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, comparative_jurists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, internally consistent method for deriving law in the absence of directly governing text: rather than leaving gaps unresolved or forcing distant analogies, Medinan communal practice and public-interest reasoning fill gaps with living, socially-grounded precedent, giving jurists a workable and locally adaptable toolkit.
% TRANSFER_FUNCTION: Moves interpretive authority and legal legitimacy toward the historical practice of Medina and toward locally dominant custom, and away from communities whose own regional practices or stricter textual readings would otherwise carry equal or greater evidentiary weight.
% ABSENT_VOICES: Jurists and communities outside the Hijaz at the formative period, and later non-Maliki Muslim populations living under Maliki-administered law, had no role in constituting what counted as 'amal ahl al-Madina, yet are bound by rulings derived from it; textualist dissenters within the tradition are also structurally absent from adjudicative forums once precedent hardens.
% DISAPPEARANCE_RATIONALE: If Medinan-practice-as-source and maslaha mursala were withdrawn as valid sources overnight, a large body of Maliki fiqh on transactions, family law, and public administration would lose its jurisprudential grounding and would need re-derivation from hadith and qiyas alone — producing substantively different rulings in areas (like certain commercial customs and public-interest-based governance measures) where Maliki method currently reaches conclusions unavailable to Shafi'i or Hanbali method.
% FOUNDING_PROBLEM: In the first Islamic century, jurists in Medina faced legal questions where explicit Quranic or hadith text was silent or where solitary hadith reports conflicted with the observed, continuous practice of the community that had lived alongside the Prophet and his Companions; a method was needed to adjudicate whether communal continuity itself constituted reliable evidence of prophetic teaching.
% FOUNDING_PROBLEM_CORROBORATION: Maliki jurists and the school's own biographical literature attest the founding problem remains live wherever text is silent and communal practice persists. Comparative jurists and Shafi'i-tradition critics (notably al-Shafi'i's own recorded objections in Kitab al-Umm) attest from outside the Maliki school that the reliance on 'amal ahl al-Madina risks conflating later Medinan custom with authenticated prophetic practice, and that the evidentiary problem the doctrine claims to solve is at least partly a retrospective justification for a regionally favored legal culture.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__maliki_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__maliki_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__maliki_reading_tests).
:- end_tests(usul_al_fiqh_method__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 by interval end) rather than low, reflecting that the doctrine does real coordination work (filling genuine textual gaps with a coherent method) while also structurally privileging one regional community's custom over others without their participation in constituting that privilege — this is the tangled-rope signature: coordination function plus asymmetric extraction through the same structural channel. Suppression is moderate (0.32): Maliki method does not physically coerce adherence, but once 'amal ahl al-Madina and maslaha-based rulings are codified into a school's fiqh corpus and administered by qadis, textualist dissent has little institutional forum. Theater ratio is low-to-moderate (0.18) and rising slowly, consistent with a living jurisprudential method rather than a hollowed-out formalism — the doctrine continues to do genuine interpretive work across the interval, though administrative codification adds some performative citation practice over centuries.
 *
 * DIRECTIONALITY LOGIC:
 *   The Medinan juristic lineage and regional qadis sit near the beneficiary end: they administer the source hierarchy and gain interpretive authority and discretion from it. Local customary authorities benefit structurally because their 'urf gains legal standing it would lack under more textually restrictive methods. Non-Medinan legal communities and textualist dissenters sit nearer the target end: they bear a hierarchy that was not built with their participation and that constrains their own competing legal reasoning, with limited exit given that Maliki jurisdiction is often coextensive with state legal administration in Maliki-majority regions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to treat communal continuity as evidence of prophetic practice where text is silent — remains genuinely live wherever Maliki-administered communities encounter novel questions unaddressed by explicit text; this prevents a blanket 'pure extraction' reading. But the founding_problem_status is authored as contested rather than live, because critics both within and outside the tradition (echoing al-Shafi'i's own historical objection) argue that later Medinan custom is not reliably continuous with prophetic-era practice, meaning part of what the doctrine collects is legitimacy for a regionally favored legal culture rather than genuine transmission-fidelity. The tangled_rope classification holds both facts simultaneously rather than forcing a single verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amal_authenticity_ambiguity,
    'Is the ''amal ahl al-Madina invoked by later Maliki jurists a reliable transmission of prophetic-era practice, or a retrospective legitimation of custom that accumulated in Medina after the formative period and diverged from it?',
    'Historical-critical reconstruction comparing early attestations of specific Medinan practices against the dates and provenance of the sources reporting them, cross-checked against competing hadith transmission chains for the same rulings.',
    'If ''amal reliably tracks prophetic-era practice, the doctrine functions closer to a genuine coordination mechanism (filling textual gaps with authentic evidence). If it substantially diverged, the doctrine functions closer to extraction — laundering later regional custom as prophetic authority, which would push the classification toward snare for communities bound by rulings so derived.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(amal_authenticity_ambiguity, empirical, 'Whether ''amal ahl al-Madina is authentic transmission or retrospective legitimation of accumulated custom.').

omega_variable(
    maslaha_mursala_boundary_indeterminacy,
    'What principled boundary distinguishes maslaha mursala (legitimate unrestricted public interest reasoning) from unconstrained juristic discretion dressed in public-interest language?',
    'Comparative analysis of documented maslaha-based rulings across centuries of Maliki practice, checking whether the reasoning is falsifiable/constrained by identifiable criteria or functions as an open license for outcome-driven ruling.',
    'A well-bounded maslaha doctrine supports the coordination-function claim; an unbounded one would indicate the extraction component is larger than the current ε estimate reflects, since discretion without a check is a channel for arbitrary transfer toward whoever administers it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_mursala_boundary_indeterminacy, conceptual, 'Whether maslaha mursala has a principled limiting criterion or amounts to unconstrained discretion.').

omega_variable(
    kernel_reading_selection_grounds,
    'Given that the underlying kernel (how to derive law where text is silent) admits at least four structurally distinct, internally coherent readings (Hanafi, Shafi''i, Maliki, Hanbali), what independent grounds — beyond regional/historical contingency of where each school took root — justify treating the Maliki reading as authoritative for communities that had no part in constituting Medinan ''amal?',
    'This is a conceptual/framing question rather than an empirical one: it would be addressed by comparative usul al-fiqh scholarship examining whether any meta-principle adjudicates between the four schools'' source hierarchies, or whether the selection is irreducibly a matter of historical transmission and communal adherence rather than demonstrated superiority.',
    'If no independent adjudicating ground exists, the elevation of one reading over others for a given population is closer to historical-institutional path-dependency than to a resolved jurisprudential question — relevant to how much weight non-Medinan/non-Maliki communities'' objections should carry in the disappearance and victim analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_grounds, conceptual, 'Whether any principled ground beyond historical contingency selects the Maliki reading over its siblings for a given community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__maliki_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__maliki_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(usul_tr_t20, usul_al_fiqh_method__maliki_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(usul_tr_t40, usul_al_fiqh_method__maliki_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(usul_tr_t60, usul_al_fiqh_method__maliki_reading, theater_ratio, 60, 0.14).
narrative_ontology:measurement(usul_tr_t80, usul_al_fiqh_method__maliki_reading, theater_ratio, 80, 0.16).
narrative_ontology:measurement(usul_tr_t100, usul_al_fiqh_method__maliki_reading, theater_ratio, 100, 0.18).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__maliki_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(usul_be_t20, usul_al_fiqh_method__maliki_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(usul_be_t40, usul_al_fiqh_method__maliki_reading, base_extractiveness, 40, 0.31).
narrative_ontology:measurement(usul_be_t60, usul_al_fiqh_method__maliki_reading, base_extractiveness, 60, 0.34).
narrative_ontology:measurement(usul_be_t80, usul_al_fiqh_method__maliki_reading, base_extractiveness, 80, 0.36).
narrative_ontology:measurement(usul_be_t100, usul_al_fiqh_method__maliki_reading, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__maliki_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(usul_su_t20, usul_al_fiqh_method__maliki_reading, suppression_requirement, 20, 0.24).
narrative_ontology:measurement(usul_su_t40, usul_al_fiqh_method__maliki_reading, suppression_requirement, 40, 0.27).
narrative_ontology:measurement(usul_su_t60, usul_al_fiqh_method__maliki_reading, suppression_requirement, 60, 0.29).
narrative_ontology:measurement(usul_su_t80, usul_al_fiqh_method__maliki_reading, suppression_requirement, 80, 0.31).
narrative_ontology:measurement(usul_su_t100, usul_al_fiqh_method__maliki_reading, suppression_requirement, 100, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__maliki_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__maliki_reading, 0.12).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the usul_al_fiqh_method kernel, each a separate constraint file with independently authored ε and structural data. The Maliki reading is distinguished by elevating 'amal ahl al-Madina and maslaha mursala to independent-source status; the Hanafi reading solves the same textual-silence problem via expansive qiyas and istihsan; the Shafi'i reading via hadith-authentication primacy and restricted qiyas; the Hanbali reading via maximal textual restriction and sadd al-dhara'i. None of the four files should be read as measuring 'the same constraint' — the ε-invariance principle requires decomposition precisely because these readings differ in beneficiary/victim structure and in how they resolve gaps in text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
