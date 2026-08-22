% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: usul_al_fiqh_method__maliki_reading
 *   human_readable: Maliki Source Hierarchy: Medinan Practice, Public Interest, and Custom
 *   domain: legal/jurisprudential/religious
 *
 * SUMMARY:
 *   A methodological settlement in Maliki jurisprudence assigns independent
 *   evidentiary weight to the continuous practice of Medina's Muslim
 *   community, admits public interest unaddressed by texts as a source, and
 *   integrates local custom where it does not contradict revelation. The
 *   arrangement solved a real problem — governing a vast, diverse region
 *   under a textually incomplete revelation — while concentrating
 *   adjudicative authority in a juristic class whose standing depends on
 *   administering the hierarchy. Its center of gravity migrated from Medina
 *   to the Maghrib and al-Andalus, where custom and public interest did the
 *   operative work while Medinan practice increasingly supplied legitimation.
 *   The manifest's stated victim ('universalist textualism') is a doctrine,
 *   not an actor; per the proposition/actor split it is modeled through the
 *   real actors who bear its costs (hadith specialists, textual-evidence
 *   litigants), while the doctrinal content rides in vindicated_propositions.
 *   Claim/metric independence is deliberate: the reading is CLAIMED as
 *   tangled_rope and the metrics are authored to describe the arrangement's
 *   actual operation across its classical arc. KEY AGENTS (by structural
 *   relationship): - maliki_jurisprudential_class: Agenda setter
 *   (institutional/constrained) — administers the source hierarchy, collects
 *   authority and patronage - medinan_practice_community: Primary beneficiary
 *   (organized/mobile) — its practice carries evidentiary weight -
 *   maghribi_andalusian_local_communities: Beneficiary/payer
 *   (organized/constrained) — customs legalized, lives adjudicated -
 *   hadith_specialist_transmitters: Payer (moderate/identity_locked) —
 *   authentication expertise discounted - transregional_traders: Payer
 *   (powerful/mobile) — contracts judged by unfamiliar local custom -
 *   non_customary_litigants: Payer (powerless/trapped) — textual evidence
 *   discounted in court - rival_madhhab_jurists: Excluded
 *   (institutional/constrained) — objections carry no official weight -
 *   ruling_dynasties: Agenda setter (institutional/arbitrage) — enforcement
 *   oscillated with dynastic politics - comparative_law_historians:
 *   Analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, 0.57).
domain_priors:suppression_score(usul_al_fiqh_method__maliki_reading, 0.58).
domain_priors:theater_ratio(usul_al_fiqh_method__maliki_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, extractiveness, 0.57).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__maliki_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__maliki_reading, "Maliki Source Hierarchy: Medinan Practice, Public Interest, and Custom").
narrative_ontology:topic_domain(usul_al_fiqh_method__maliki_reading, "legal/jurisprudential/religious").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__maliki_reading, 'dfe47894-7f1a-4a09-9bd4-09036ab6eedb').
narrative_ontology:cs_kernel_codification('dfe47894-7f1a-4a09-9bd4-09036ab6eedb', formalized).
narrative_ontology:cs_authority_grounding('dfe47894-7f1a-4a09-9bd4-09036ab6eedb', lineage).
narrative_ontology:cs_interpretation_layer_present('dfe47894-7f1a-4a09-9bd4-09036ab6eedb').
narrative_ontology:cs_reading_relation('dfe47894-7f1a-4a09-9bd4-09036ab6eedb', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('dfe47894-7f1a-4a09-9bd4-09036ab6eedb', usul_al_fiqh_method__shafii_reading, influences).
narrative_ontology:cs_reading_relation('dfe47894-7f1a-4a09-9bd4-09036ab6eedb', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('dfe47894-7f1a-4a09-9bd4-09036ab6eedb', foundational, continuous_community_practice_evidentiary).
narrative_ontology:cs_axiom_status(continuous_community_practice_evidentiary, holdable).
narrative_ontology:cs_axiom_grounding('dfe47894-7f1a-4a09-9bd4-09036ab6eedb', continuous_community_practice_evidentiary, empirically_contingent).
narrative_ontology:cs_axiom('dfe47894-7f1a-4a09-9bd4-09036ab6eedb', foundational, public_interest_independent_source).
narrative_ontology:cs_axiom_status(public_interest_independent_source, holdable).
narrative_ontology:cs_axiom_grounding('dfe47894-7f1a-4a09-9bd4-09036ab6eedb', public_interest_independent_source, instrumental).
narrative_ontology:cs_axiom('dfe47894-7f1a-4a09-9bd4-09036ab6eedb', secondary, custom_binding_absent_textual_conflict).
narrative_ontology:cs_axiom_status(custom_binding_absent_textual_conflict, holdable).
narrative_ontology:cs_axiom_grounding('dfe47894-7f1a-4a09-9bd4-09036ab6eedb', custom_binding_absent_textual_conflict, conventional).
narrative_ontology:cs_reference_frame('dfe47894-7f1a-4a09-9bd4-09036ab6eedb', medinan_prophetic_continuity).
narrative_ontology:cs_drift_state('dfe47894-7f1a-4a09-9bd4-09036ab6eedb', classical_consolidation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dfe47894-7f1a-4a09-9bd4-09036ab6eedb', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, medinan_practice_community).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, maghribi_andalusian_local_communities).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, maliki_jurisprudential_class).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, hadith_specialist_transmitters).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, non_customary_litigants).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, transregional_traders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, maghribi_andalusian_local_communities).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, medinan_practice_sunnah_equivalence).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, maslaha_mursala_validity).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, urf_noncontradiction_integration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Muftis, qadis, and madrasa teachers who apply the source hierarchy in opinions, judgments, and instruction. They decide when continuous Medinan practice outweighs a transmitted report, when public interest justifies a ruling, and which customs qualify for integration. Teaching posts, judicial appointments, and endowment patronage flow through adherence to the method; leaving it means forfeiting standing built over a career and often a family lineage of scholarship.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, maliki_jurisprudential_class, agenda_setter,
    institutional, generational, constrained, continental).

% The Muslims of Medina whose continuous practice since the Prophet's era is treated as probative evidence of revealed law even where no transmitted report exists. The designation brings religious honor and makes their city the touchstone for one major school's legal reasoning; as the school's demographic center moved west, the honor remained while day-to-day citation shifted to scholars invoking them at a distance.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, medinan_practice_community, beneficiary,
    organized, generational, mobile, local).

% Muslim communities of North Africa and al-Andalus whose market conventions, marriage arrangements, and water-sharing customs are absorbed into enforceable law where the texts are silent. Their ways of life gain legal protection and predictability; the same communities then live under jurists' judgments about which of their customs qualify and which are struck down.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, maghribi_andalusian_local_communities, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__maliki_reading, maghribi_andalusian_local_communities, payer).

% Transmitters and critics of prophetic reports whose authentication work does not guarantee legal force: where an authenticated report conflicts with continuous Medinan practice, the report can lose. Their vocation is their identity — trained from childhood in chains of transmission — so the discounting of their evidence strikes at the core of their craft rather than a peripheral interest.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, hadith_specialist_transmitters, payer,
    moderate, biographical, identity_locked, continental).

% Merchants moving goods between regions whose contracts are judged by local custom when disputes land in Maliki courts. Conventions valid in one port may fail in another; they can route trade toward rival jurisdictions or structure contracts around known local custom, at the price of legal uncertainty and occasional loss.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, transregional_traders, payer,
    powerful, biographical, mobile, global).

% Parties to disputes — often the poor, newcomers, or members of minority communities — whose claims rest on textual arguments or custom of unfamiliar origin. Once a case reaches a qadi they cannot take it elsewhere; where the opposing side's practice is entrenched, their textual evidence carries less weight.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, non_customary_litigants, payer,
    powerless, immediate, trapped, local).

% Hanafi, Shafi'i, and Hanbali colleagues who deny that regional practice or unrestricted public interest count as independent sources. They press objections in treatises and debate, but inside Maliki courts their methodological premises carry no official weight; their critique circulates in the very cities the method administers.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, rival_madhhab_jurists, excluded,
    institutional, generational, constrained, continental).

% Successive North African and Andalusian dynasties that appointed qadis, endowed madrasas, and decided whether the school's method received state backing. The Almoravids enforced conformity to it; the Almohads withdrew backing and persecuted its adherents for a period; the Marinids restored and endowed it. Each dynasty could move enforcement up or down through appointment and patronage.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, ruling_dynasties, agenda_setter,
    institutional, biographical, arbitrage, continental).

% Modern historians of Islamic law comparing the four Sunni source hierarchies, reconstructing what Medinan practice actually consisted of, and tracing how custom entered positive law. They owe allegiance to no school and can see the whole structure from outside.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, comparative_law_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__maliki_reading, maliki_jurisprudential_class).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__maliki_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, teachable procedure for deriving rulings where revelation is silent or ambiguous, so that courts across a vast region reach consistent decisions and local practice remains legally cognizable instead of being overridden by distant textual reasoning.
% TRANSFER_FUNCTION: Moves adjudicative authority, teaching posts, and endowment patronage to the juristic class; moves customary norms from informal community practice into enforceable law; and discounts the legal force of transmitted reports and textual arguments that conflict with entrenched practice.
% ABSENT_VOICES: Rival-school jurists object from outside the courtroom; hadith specialists object from the tradition's margins; ordinary litigants least positioned to invoke established custom have no procedural voice in deciding which customs count.
% DISAPPEARANCE_RATIONALE: If the source hierarchy vanished overnight, Maliki-region courts would have no procedure for text-silent cases: either textualist reasoning would override local practice wholesale, severing law from the life it governs, or each judge would improvise and rulings would fragment. Judicial appointments, madrasa curricula, and endowment-funded teaching posts are all organized around the method.
% FOUNDING_PROBLEM: Governing a rapidly expanding community under a revelation that does not address most practical questions, while keeping law continuous with prophetic practice as the first community lived it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: al-Shafi'i's own Risala concedes the problem of textual silence while disputing the Medinan solution, and modern academic historians of Islamic law attest both the founding problem's persistence and the decay of the specific unbroken-transmission premise. The strength of the Medinan-transmission claim itself is attested mainly by Maliki authorities — that asymmetry is itself signal.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__maliki_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__maliki_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__maliki_reading, 0.57, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__maliki_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The arrangement opens moderately extractive (0.32 at Malik's teaching era, when the method competed openly with rivals in Medina) and ends at 0.57 as state backing concentrated adjudicative authority. Suppression (0.58 end-state) reflects enforcement through qadi appointment, madrasa curricula, and patronage rather than raw coercion — though the Almoravid era added book-burning and forced conformity. Theater stays low-to-moderate (0.31): the method genuinely decides cases, but invocation of Medina's golden-age practice grew more formulaic as operative work shifted to Maghribi custom. Accessibility collapse sits at 0.50: three rival Sunni hierarchies remained live elsewhere, but inside Maliki jurisdictions official adjudication foreclosed them. Resistance (0.55) was sustained — al-Shafi'i's direct attack on 'amal ahl al-Madina, Hanbali resistance to expansive maslaha, and the Almohad interlude. All three series run on one shared nine-point grid (761–1276) so every metric is authored at every examined time point. The series show one full cycle: steady institutionalization to 1060, enforcement collapse under Almohad rule (suppression_requirement 0.55→0.30, extractiveness dipping as state rents vanished), then Marinid restoration. The oscillation is driven by dynastic politics rather than engineered intermittent reinforcement, though the memory of persecution reinforced post-restoration conformity — a partial reinforcement effect flagged in the omegas. On coalition potential: the powerless seat (non_customary_litigants) lacks a natural coalition vehicle because grievances arise case-by-case in individual proceedings, which is precisely why their position persisted despite numbers.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats the hierarchy is the school's living method — the thing that makes rulings possible at all; from the payer seats it operates as a discount on their evidence and an entrenchment of others' custom. The hadith specialist and the non-customary litigant experience the rule that practice outweighs the report as a loss; the jurist experiences the same rule as fidelity to the Prophet's community. The engine computes this divergence from power, exit, and directional data — the divergence is the finding, not noise.
 *
 * DIRECTIONALITY LOGIC:
 *   The juristic class is declared beneficiary and agenda-setter: it collects authority and patronage through the hierarchy, and its constrained exit deepens attachment (damped d, near the beneficiary end). The Medinan practice community and the Maghribi/Andalusian communities sit near full beneficiary: the arrangement subsidizes the legal force of their norms. Hadith specialists and non-customary litigants sit near full target: the arrangement taxes the force of their evidence, and the litigants cannot exit at all. Transregional traders are declared victims but hold mobile exit — the derivation should damp their effective extraction toward symmetric, since they can reroute trade. Ruling dynasties oscillate between enforcer and attacker across the interval; their net position is unstable, which the temporal series registers as the suppression cycle. No directionality_overrides are used: the beneficiary/victim declarations plus exit options produce the correct relationships, including the trader damping.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both mislabels. Reading the arrangement as pure extraction misses the genuine coordination function: texts are silent on most practical questions, and some procedure must bind region-wide courts while keeping local life legally cognizable — abolish the hierarchy and the world rearranges. Reading it as pure rope misses the asymmetric extraction: authority rents concentrate in the juristic class, custom entrenches whoever's practices qualified, and textual-evidence bearers pay through discounted force. Tangled rope holds both truths. Mandatrophy is not resolved: the founding problem (text-silence) remains live, so the arrangement has not outlived its mandate — though the specific Medinan-transmission premise has decayed, which the drift_state and omegas track separately from the mandate question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medinan_transmission_authenticity,
    'Was Medinan practice actually an unbroken transmission from the Prophet, or did it absorb post-prophetic development (Umayyad administration, local adaptation) before Malik codified it?',
    'Historical-critical reconstruction comparing Medinan practice reports against the hadith corpus and the administrative history of the Umayyad period.',
    'If the chain breaks, ''amal ahl al-Madina loses its evidentiary foundation and the reading collapses toward report-primacy (shafii-shaped), re-sorting every beneficiary and victim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medinan_transmission_authenticity, empirical, 'Whether the Medinan-practice premise is historically sound.').

omega_variable(
    maslaha_scope_boundary,
    'Where is the line between valid public interest (maslaha mursala) and condemnable innovation (bid''a) — and who draws it?',
    'Analysis of accepted versus rejected public-interest invocations across Maliki history (compilation of the Quran accepted; many innovations blocked via sadd al-dhara''i) tested against the jurists'' own stated criteria.',
    'If the boundary tracks juristic-class interests, the public-interest channel functions as discretionary authority expansion; if it tracks principled criteria, it is a genuine safety valve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_scope_boundary, conceptual, 'Boundary stability of the public-interest source.').

omega_variable(
    urf_entrenchment_distribution,
    'Does custom-integration track community welfare generally, or the customs of the merchants and families best positioned to have their practices recorded and defended?',
    'Compare rulings validating custom across class lines — whose market conventions, marriage arrangements, and water rights were integrated versus struck down.',
    'If elite custom dominates, the custom channel transfers legal force upward within communities and the effective victim set widens beyond outsiders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(urf_entrenchment_distribution, empirical, 'Distributional question of whose custom counts.').

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is one reading of the usul_al_fiqh_method kernel; how would the classification shift under a sibling reading?',
    'Generate the sibling stories (hanafi, shafii, hanbali) and compare computed per-seat types; the disagreement is located in the evidentiary weight assigned to extra-textual sources.',
    'Under the shafii_reading, Medinan practice becomes the constrained party and hadith specialists the beneficiaries — the directionality map inverts while the underlying community stays fixed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer-frame position of this story within the four-reading kernel contest.').

omega_variable(
    suppression_source_ambiguity,
    'Is the measured suppression primarily institutional (appointment control, patronage, episodic persecution) or internalized (juristic formation that renders rival methods unthinkable to trained jurists)?',
    'Post-persecution trajectory analysis: the Maliki method survived Almohad suppression through private transmission and rebounded immediately, suggesting substantial internalized commitment atop institutional enforcement.',
    'If internalized, suppression outlasts enforcement infrastructure and the arrangement resists dynastic withdrawal; if purely institutional, removal of state backing collapses it, as the 1120–1180 trough nearly demonstrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_source_ambiguity, conceptual, 'Structural versus internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__maliki_reading, 761, 1276).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t761, usul_al_fiqh_method__maliki_reading, theater_ratio, 761, 0.1).
narrative_ontology:measurement_basis(usul_tr_t761, observed).
narrative_ontology:measurement(usul_tr_t820, usul_al_fiqh_method__maliki_reading, theater_ratio, 820, 0.13).
narrative_ontology:measurement_basis(usul_tr_t820, observed).
narrative_ontology:measurement(usul_tr_t900, usul_al_fiqh_method__maliki_reading, theater_ratio, 900, 0.17).
narrative_ontology:measurement_basis(usul_tr_t900, observed).
narrative_ontology:measurement(usul_tr_t980, usul_al_fiqh_method__maliki_reading, theater_ratio, 980, 0.21).
narrative_ontology:measurement_basis(usul_tr_t980, observed).
narrative_ontology:measurement(usul_tr_t1060, usul_al_fiqh_method__maliki_reading, theater_ratio, 1060, 0.25).
narrative_ontology:measurement_basis(usul_tr_t1060, observed).
narrative_ontology:measurement(usul_tr_t1120, usul_al_fiqh_method__maliki_reading, theater_ratio, 1120, 0.22).
narrative_ontology:measurement_basis(usul_tr_t1120, observed).
narrative_ontology:measurement(usul_tr_t1180, usul_al_fiqh_method__maliki_reading, theater_ratio, 1180, 0.24).
narrative_ontology:measurement_basis(usul_tr_t1180, observed).
narrative_ontology:measurement(usul_tr_t1240, usul_al_fiqh_method__maliki_reading, theater_ratio, 1240, 0.28).
narrative_ontology:measurement_basis(usul_tr_t1240, observed).
narrative_ontology:measurement(usul_tr_t1276, usul_al_fiqh_method__maliki_reading, theater_ratio, 1276, 0.31).
narrative_ontology:measurement_basis(usul_tr_t1276, observed).

% Extraction over time
narrative_ontology:measurement(usul_be_t761, usul_al_fiqh_method__maliki_reading, base_extractiveness, 761, 0.32).
narrative_ontology:measurement_basis(usul_be_t761, observed).
narrative_ontology:measurement(usul_be_t820, usul_al_fiqh_method__maliki_reading, base_extractiveness, 820, 0.38).
narrative_ontology:measurement_basis(usul_be_t820, observed).
narrative_ontology:measurement(usul_be_t900, usul_al_fiqh_method__maliki_reading, base_extractiveness, 900, 0.43).
narrative_ontology:measurement_basis(usul_be_t900, observed).
narrative_ontology:measurement(usul_be_t980, usul_al_fiqh_method__maliki_reading, base_extractiveness, 980, 0.47).
narrative_ontology:measurement_basis(usul_be_t980, observed).
narrative_ontology:measurement(usul_be_t1060, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1060, 0.51).
narrative_ontology:measurement_basis(usul_be_t1060, observed).
narrative_ontology:measurement(usul_be_t1120, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1120, 0.46).
narrative_ontology:measurement_basis(usul_be_t1120, observed).
narrative_ontology:measurement(usul_be_t1180, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1180, 0.49).
narrative_ontology:measurement_basis(usul_be_t1180, observed).
narrative_ontology:measurement(usul_be_t1240, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1240, 0.54).
narrative_ontology:measurement_basis(usul_be_t1240, observed).
narrative_ontology:measurement(usul_be_t1276, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1276, 0.57).
narrative_ontology:measurement_basis(usul_be_t1276, observed).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t761, usul_al_fiqh_method__maliki_reading, suppression_requirement, 761, 0.22).
narrative_ontology:measurement_basis(usul_su_t761, observed).
narrative_ontology:measurement(usul_su_t820, usul_al_fiqh_method__maliki_reading, suppression_requirement, 820, 0.28).
narrative_ontology:measurement_basis(usul_su_t820, observed).
narrative_ontology:measurement(usul_su_t900, usul_al_fiqh_method__maliki_reading, suppression_requirement, 900, 0.36).
narrative_ontology:measurement_basis(usul_su_t900, observed).
narrative_ontology:measurement(usul_su_t980, usul_al_fiqh_method__maliki_reading, suppression_requirement, 980, 0.42).
narrative_ontology:measurement_basis(usul_su_t980, observed).
narrative_ontology:measurement(usul_su_t1060, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1060, 0.55).
narrative_ontology:measurement_basis(usul_su_t1060, observed).
narrative_ontology:measurement(usul_su_t1120, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1120, 0.3).
narrative_ontology:measurement_basis(usul_su_t1120, observed).
narrative_ontology:measurement(usul_su_t1180, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1180, 0.4).
narrative_ontology:measurement_basis(usul_su_t1180, observed).
narrative_ontology:measurement(usul_su_t1240, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1240, 0.52).
narrative_ontology:measurement_basis(usul_su_t1240, observed).
narrative_ontology:measurement(usul_su_t1276, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1276, 0.58).
narrative_ontology:measurement_basis(usul_su_t1276, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__maliki_reading, identity_coordination).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'usul al-fiqh' covers four structurally distinct source-hierarchy settlements, each with its own epsilon, beneficiary/victim sets, and failure modes. This member (maliki_reading) elevates regional practice and custom to source status; the shafii_reading upstream claim (report authentication as prerequisite) is frequently cited AGAINST this member's Medinan-practice premise, making the family linkage bidirectional in argument even though the edge here records this story's structural influence on its siblings. Each reading is authored as a separate file; none hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
