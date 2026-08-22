% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__contextual_defensive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__contextual_defensive, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: quran_9_5_scope__contextual_defensive
 *   human_readable: Verse 9:5 Contextual-Defensive Reading (Treaty-Bounded, Defensive-Only Warfare Doctrine)
 *   domain: religious/hermeneutic/political-theological
 *
 * SUMMARY:
 *   Quran 9:5 — the so-called 'Sword Verse' — proclaims fighting against the
 *   polytheists 'wherever you find them' once the sacred months pass. The
 *   contextual_defensive reading constrains that proclamation four ways: it
 *   is addressed to the specific seventh-century Medinan constellation of
 *   treaty-breaking polytheist tribes; it does not cancel the earlier
 *   peaceful-coexistence verses; treaty obligations retain priority; and
 *   warfare under it is defensive or treaty-enforcing only. The standing
 *   arrangement under contest — and the epsilon referent for this file — is
 *   that doctrine as an operative rule of Islamic international law: who may
 *   be fought, on what trigger, under what prior conditions. This file is ONE
 *   reading of kernel quran_9_5_scope; the abrogating_universal and
 *   progressive_synthesis readings are separate constraints with their own
 *   epsilon, victim sets, and classifications, linked through
 *   network.affects_constraints. Nothing from those readings is averaged into
 *   this file's metrics. KEY AGENTS (by structural relationship): -
 *   islamic_exegetical_establishment: agenda-setter
 *   ([institutional]/[constrained]) — adjudicates the verse's scope and
 *   transmits the reading - integrationist_muslim_states: primary beneficiary
 *   ([institutional]/[mobile]) — collects diplomatic legitimacy and
 *   minority-security dividends - religious_minorities_muslim_societies:
 *   beneficiary ([moderate]/[constrained]) — security of person and worship
 *   rides on the protected coexistence norms - general_muslim_populations:
 *   beneficiary ([moderate]/[constrained]) — lived religion certified as
 *   peacefully normative - treaty_breaking_aggressor_parties: payer
 *   ([organized]/[constrained]) — bears enforcement response after their own
 *   breach - abrogationist_jihad_doctrinaires: excluded rival
 *   ([organized]/[identity_locked]) — holds the foreclosed sibling reading
 *   outside the adjudication - academic_historians_islamic_origins:
 *   analytical observer ([analytical]/[analytical]) — attests the occasion
 *   and the reading's historical career from outside confessional
 *   adjudication
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__contextual_defensive, 0.22).
domain_priors:suppression_score(quran_9_5_scope__contextual_defensive, 0.3).
domain_priors:theater_ratio(quran_9_5_scope__contextual_defensive, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, extractiveness, 0.22).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__contextual_defensive, rope).
narrative_ontology:human_readable(quran_9_5_scope__contextual_defensive, "Verse 9:5 Contextual-Defensive Reading (Treaty-Bounded, Defensive-Only Warfare Doctrine)").
narrative_ontology:topic_domain(quran_9_5_scope__contextual_defensive, "religious/hermeneutic/political-theological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__contextual_defensive, 'defe360c-539c-4987-95c5-788eab24a0fa').
narrative_ontology:cs_kernel_codification('defe360c-539c-4987-95c5-788eab24a0fa', fixed_text).
narrative_ontology:cs_authority_grounding('defe360c-539c-4987-95c5-788eab24a0fa', lineage).
narrative_ontology:cs_interpretation_layer_present('defe360c-539c-4987-95c5-788eab24a0fa').
narrative_ontology:cs_reading_relation('defe360c-539c-4987-95c5-788eab24a0fa', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('defe360c-539c-4987-95c5-788eab24a0fa', quran_9_5_scope__progressive_synthesis, coexists_with).
narrative_ontology:cs_axiom('defe360c-539c-4987-95c5-788eab24a0fa', foundational, no_abrogation_of_peaceful_verses).
narrative_ontology:cs_axiom_status(no_abrogation_of_peaceful_verses, holdable).
narrative_ontology:cs_axiom_grounding('defe360c-539c-4987-95c5-788eab24a0fa', no_abrogation_of_peaceful_verses, theological).
narrative_ontology:cs_axiom('defe360c-539c-4987-95c5-788eab24a0fa', foundational, warfare_requires_prior_aggression_or_treaty_breach).
narrative_ontology:cs_axiom_status(warfare_requires_prior_aggression_or_treaty_breach, holdable).
narrative_ontology:cs_axiom_grounding('defe360c-539c-4987-95c5-788eab24a0fa', warfare_requires_prior_aggression_or_treaty_breach, deontological).
narrative_ontology:cs_reference_frame('defe360c-539c-4987-95c5-788eab24a0fa', medinan_treaty_context_frame).
narrative_ontology:cs_drift_state('defe360c-539c-4987-95c5-788eab24a0fa', contemporary_postcolonial_contestation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('defe360c-539c-4987-95c5-788eab24a0fa', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__contextual_defensive, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, integrationist_muslim_states).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, religious_minorities_muslim_societies).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, general_muslim_populations).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, treaty_breaking_aggressor_parties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, islamic_exegetical_establishment).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, contextual_revelation_principle).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, treaty_fidelity_doctrine).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, defensive_war_limitation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Transmits and adjudicates the interpretation of Quran 9:5 through commentarial chains, legal manuals, and formal opinions. Maintains the methodological rules governing claims that one verse cancels another, and issues rulings confining the verse's battlefield provisions to parties that have broken treaties or opened hostilities. Its standing depends on continuity with the interpretive tradition: abandoning the contextual reading would contradict centuries of its own transmitted commentary, while defending it commits it to continuous rebuttal of rival readers.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, islamic_exegetical_establishment, agenda_setter,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__contextual_defensive, islamic_exegetical_establishment, beneficiary).

% Muslim-majority states that sign treaties, join alliances, protect resident non-Muslim communities, and conduct ordinary diplomacy. They invoke the contextual-defensive doctrine to certify that their treaty commitments bind and that war requires a prior breach or attack. The doctrine supplies them with diplomatic legitimacy and domestic minority stability; they can and sometimes do shift interpretive framing when alliances change.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, integrationist_muslim_states, beneficiary,
    institutional, generational, mobile, global).

% Christian, Jewish, and other non-Muslim communities living under Muslim-majority governance. Their security of person, property, and worship is underwritten by the coexistence norms this reading protects. Emigration is possible but costly, splitting families and forfeiting livelihoods; their practical option set runs through petitioning, litigation, and communal negotiation.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, religious_minorities_muslim_societies, beneficiary,
    moderate, generational, constrained, regional).

% The mass of believers whose lived religion includes neighborly coexistence, commerce, and intermarriage with non-Muslims. They bear the reputational and security costs whenever rival war-doctrines dominate public perception, and they benefit when the prevailing doctrine certifies their ordinary peaceful life as religiously sound. Individual leverage over doctrinal outcomes is limited; influence runs through scholars, media, and communal institutions.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, general_muslim_populations, beneficiary,
    moderate, biographical, constrained, global).

% Polities, tribes, or armed groups that have signed agreements and then breached them or initiated hostilities. Under this reading they alone become lawful objects of military response, and only after their own violation; restoring compliance and restitution reopens protected status. Their exposure is conditional on conduct rather than identity, but once breach occurs the response follows unless they stand down.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, treaty_breaking_aggressor_parties, payer,
    organized, immediate, constrained, regional).

% Ideologues, preachers, and armed movements holding that verse 9:5 cancels the earlier peaceful verses and grounds a standing offensive obligation. They stand outside the contextual reading's adjudicative processes — consensus letters, scholarly unions, state-sponsored opinion councils — and reject those bodies' authority. Their movement identity is built on the rival thesis; adopting the contextual reading would dissolve the movement's core claim, so exit is not a live option for committed members. The contextual doctrine's spread directly erodes their recruitment narrative.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, abrogationist_jihad_doctrinaires, excluded,
    organized, biographical, identity_locked, global).

% University-based historians and philologists studying the Medinan period, treaty documents, and the occasions of revelation. They attest the specific seventh-century circumstances the verse addresses and the later career of its interpretation, from outside confessional adjudication. Their findings feed both defenders and critics of the contextual reading.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, academic_historians_islamic_origins, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__contextual_defensive, integrationist_muslim_states).
narrative_ontology:fixing_cost_class(quran_9_5_scope__contextual_defensive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines the boundary of lawful warfare for a religious polity: force is authorized only in defense or in enforcement of treaty obligations against parties that have themselves broken agreement or opened hostilities. This solves the recurring collective-action problem of when fighting is permitted — preventing perpetual-war dynamics, protecting commerce and diplomacy, and giving minority and neighboring communities enforceable security expectations.
% TRANSFER_FUNCTION: Moves the costs of armed response onto parties that have breached treaties or initiated aggression, moves the decision-point for war behind a prior-violation condition, and moves security assurance to treaty-abiding communities and minorities in exchange for political order. Would-be offensive warriors forgo a license they would hold under rival readings.
% ABSENT_VOICES: Holders of the abrogating_universal reading are structurally absent from the adjudicative bodies (consensus declarations, official opinion councils) that certify the contextual reading — they would object that the reading empties the verse of its command. Also absent: the conquered and displaced populations of the early expansionary campaigns, whose descendants would contest the defensive framing of those wars; and secular international-law scholars who object to any scriptural war-authorization. They sit outside the jurisprudential process, in academia, rival movements, and diaspora publics.
% DISAPPEARANCE_RATIONALE: If the contextual-defensive rule vanished overnight, Muslim-state warfare doctrine would reorganize around one of the rival readings — abrogationist license or historicist dissolution — minority-protection guarantees would lose their operative textual anchor, and the counter-extremism architecture built to defend coexistence norms would lose its doctrinal foundation. Treaties, minority security arrangements, and interfaith frameworks currently rest on this reading's certification.
% FOUNDING_PROBLEM: The Medinan polity faced repeated treaty breaches by allied polytheist tribes — culminating in the breach cycle that triggered the campaign on Mecca — and needed a rule that answered treachery decisively without converting every subsequent peace into a truce pending attack. The verse's proclamation followed the expiration of treaty terms after that breach cycle.
% FOUNDING_PROBLEM_CORROBORATION: Academic historiography of the Medinan treaty system (sira chronology, the Hudaybiyya aftermath, the Banu Khuza'a/Banu Bakr hostilities) corroborates the founding problem from outside the beneficiary set, as does comparative international-law scholarship on treaty-breach response, which treats the underlying problem — answering breach without licensing perpetual war — as a standing feature of any legal order. No corroboration exists, from any seat, for the claim that the verse's rule was meant to lapse; that question remains the kernel contest itself.
narrative_ontology:disappearance_verdict(quran_9_5_scope__contextual_defensive, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__contextual_defensive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__contextual_defensive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_9_5_scope__contextual_defensive, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__contextual_defensive, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__contextual_defensive_tests).
:- end_tests(quran_9_5_scope__contextual_defensive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22): the rule imposes costs almost exclusively on parties that have themselves breached agreements, and its residual costs are the foregone offensive license denied to expansionist factions plus the abuse surface left by unilateral breach-adjudication. Suppression (0.30) is authored as a raw structural property — the engine scales only extractiveness, never suppression — and reflects bounded institutional marginalization of the rival reading plus counter-extremism enforcement, not coerced retention of participants, who overwhelmingly prefer the arrangement. Theater (0.26) covers performative peace rhetoric that outruns practice in some invoking states. Accessibility collapse is low (0.25): both rival readings remain fully live and legally arguable; understanding this reading closes none of them. Resistance is substantial (0.62): abrogationist movements actively contest the reading and secular critics reject the entire genre of scriptural war-authorization, so the constraint must be continuously defended — which is exactly why the suppression_requirement series rises across the interval. All three tracked metrics run on one shared grid (points 0, 6, 12, 18, 24, 30, mapping approximately to 1995-2025 at five-year steps): extractiveness stays flat-to-slightly-rising, theater creeps up with performative coexistence diplomacy before easing slightly as verification mechanisms mature, and enforcement capacity builds sharply after the point-6 shock (the 2001 attacks) and the point-18 caliphate declaration, then plateaus. The trajectories describe a constraint under sustained defense, not one extracting through a ratchet.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the establishment seat the arrangement is fidelity: the verse read through its occasion, the tradition's methodological rules vindicated. From the integrationist-state seat it is an asset: diplomatic legitimacy and minority stability. From the minority and population seats it is protection. From the treaty-violator seat the same structure arrives as punitive force — though triggered by the violator's own conduct. The maximal divergence sits at the excluded rival seat: abrogationist doctrinaires, identity-locked by ideological fusion (the movement's self-concept is constituted by the thesis that 9:5 abrogates peace), compute this constraint as the suppression of divine command itself; if that identity frame broke — if occasion-evidence were admitted as probative within their framework — the seat would migrate toward neutral or observer positioning and the constraint's apparent suppression load would drop accordingly. Same-level institutional dynamics differentiate the two institutional seats: establishment and states hold comparable formal power, but the establishment's exit is constrained (it cannot disown its transmitted corpus) while states exit mobile (framing shifts with alliances), so identical power atoms yield different structural relationships and different computed experiences of the same doctrine.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (integrationist states, minorities, general populations) derive low directionality — the arrangement subsidizes them. The declared victim set (treaty violators) derives high directionality, amplified by constrained exit. One override is authored: power atom 'organized' to d 0.82. Rationale: the only organized-power agents in this story are the treaty violators (whose derived directionality already sits near the target pole, so 0.82 displaces it negligibly) and the abrogationist doctrinaires, who appear in no beneficiary or victim array — they are opponents, not payers — leaving the derivation chain no structural data and a mid-range fallback that would misrepresent them as symmetric. The override places them near full-target, which is their actual relationship: the constraint's normative enforcement falls directly on their legitimating claim. One known limitation, accepted deliberately: an institutional-level override would also strike the integrationist states and wreck their derived beneficiary-side directionality, so the establishment's directionality rides the canonical fallback; its dual position (collects authority rents, bears the defense burden) approximates symmetry, and the approximation is flagged here rather than forced through a distorting override.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Critics outside the tradition read the doctrine as apologetic performance — a degraded-function accusation (real function atrophied, maintenance theatrical). The structural data rebut it: the enforcement record is functional (treaty certifications, minority-protection rulings, doctrinal counter-extremism work), theater sits under a third, and the founding problem — answering treaty breach without licensing perpetual war — is live, corroborated from outside the beneficiary set by academic historiography and comparative treaty-law scholarship. Abrogationists read it as extraction wearing a peaceful mask (an eternal offensive duty hidden behind contextual talk). The data rebut that too: the victim set is behavior-triggered, not class-fixed; no seat concentrates the extraction (the states' administrative dividends are the largest flow and are modest); and the arrangement persists on participant preference, not on suppressing exit. Mandatrophy resolution: founding_problem_status=live crossed with disappearance_verdict=world_rearranges produces no dead-mandate mismatch flag — the arrangement is neither zombie nor capture, and the receipt surface (gains accruing to the administering states, fixing prohibitively costly for the tradition that would have to disown its own corpus) records the facts without asserting either pathology.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of kernel quran_9_5_scope (reading: contextual_defensive). How would the sibling readings change the structure if adopted?',
    'Corpus-level join on kernel_id comparing victim sets and epsilon across the three reading files: adoption of abrogating_universal expands the victim set to all unsubmitted polytheist polities and raises extraction sharply; adoption of progressive_synthesis dissolves the standing legal rule into historical commentary, driving binding-force extraction toward zero as the rule lapses.',
    'Classification of this file holds only under this reading; any aggregate verdict over ''what the verse commands'' is ill-formed without conditioning on the reading, and cross-reading comparison must join on kernel_id rather than averaging metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this file instantiates one of three live readings of the 9:5-scope kernel; siblings are separate constraints.').

omega_variable(
    treaty_breach_adjudication_risk,
    'Who determines that a treaty was broken, and is that determination systematically unilateral?',
    'Historical case studies of breach declarations preceding military campaigns, plus analysis of whether modern invocations route through independent arbitration or self-certification by the aggrieved polity.',
    'If breach determination is systematically unilateral, the reading''s principal abuse vector is live: declaring breach to license attack would convert the protective rule into an instrument of aggression and drive effective extraction far above the authored value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_breach_adjudication_risk, empirical, 'Self-adjudication of treaty breach is the reading''s main extractive risk surface.').

omega_variable(
    defensive_label_stretch,
    'Does ''defensive'' stretch in practice to cover preventive or expansionary campaigns, including retrospective defensive framing of the early conquests?',
    'Comparative analysis of campaign justifications against occasion-of-revelation evidence and treaty records; tracking whether modern invocations of the doctrine accompany genuinely reactive or anticipatory operations.',
    'Wide stretch inflates the theater measure and effective extraction (aggression dressed as defense); narrow stretch confirms the low-extraction profile authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defensive_label_stretch, empirical, 'Whether the defensive-only limit holds under operational framing pressure.').

omega_variable(
    suppression_mechanism_composition,
    'Is the rising enforcement intensity traced in the suppression_requirement series structural (state counter-extremism machinery, platform policing, institutional exclusion of rival readers) or internalized (community self-policing of deviant doctrine)?',
    'Post-liberalization trajectory analysis: if enforcement pressure persists where state machinery withdraws, the internalized component is confirmed; if it decays with the machinery, the structural component dominates.',
    'A large internalized share means effective suppression exceeds the structural measure — communities would carry the exclusion of rival readings even absent enforcement infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Composition of the constraint''s suppressive force between structural and internalized mechanisms.').

omega_variable(
    cs_authority_framing_underdetermination,
    'Is the authority grounding this reading the exegetical lineage (transmission-chain scholarship adjudicating the text) or the states that wield the doctrine instrumentally (institutions extracting benefit from kernel stability)?',
    'Trace the origin of scope rulings: if authoritative determinations issue from scholarly bodies and states adopt them, lineage framing holds; if states originate rulings and scholarship ratifies them, extraction framing holds.',
    'Adopting the extraction framing would reclassify the authority structure, raise the measured coupling between doctrine and state interest, and shift per-seat classifications toward enforced-hybrid readings; the lineage framing chosen here reflects that adjudication demonstrably occurs in scholarly institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_authority_framing_underdetermination, conceptual, 'Alternative commitment-system framings of the same arrangement produce different authority classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__contextual_defensive, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(q95cd_tr_t0, quran_9_5_scope__contextual_defensive, theater_ratio, 0, 0.14).
narrative_ontology:measurement(q95cd_tr_t6, quran_9_5_scope__contextual_defensive, theater_ratio, 6, 0.17).
narrative_ontology:measurement(q95cd_tr_t12, quran_9_5_scope__contextual_defensive, theater_ratio, 12, 0.21).
narrative_ontology:measurement(q95cd_tr_t18, quran_9_5_scope__contextual_defensive, theater_ratio, 18, 0.24).
narrative_ontology:measurement(q95cd_tr_t24, quran_9_5_scope__contextual_defensive, theater_ratio, 24, 0.27).
narrative_ontology:measurement(q95cd_tr_t30, quran_9_5_scope__contextual_defensive, theater_ratio, 30, 0.26).

% Extraction over time
narrative_ontology:measurement(q95cd_be_t0, quran_9_5_scope__contextual_defensive, base_extractiveness, 0, 0.17).
narrative_ontology:measurement(q95cd_be_t6, quran_9_5_scope__contextual_defensive, base_extractiveness, 6, 0.19).
narrative_ontology:measurement(q95cd_be_t12, quran_9_5_scope__contextual_defensive, base_extractiveness, 12, 0.21).
narrative_ontology:measurement(q95cd_be_t18, quran_9_5_scope__contextual_defensive, base_extractiveness, 18, 0.2).
narrative_ontology:measurement(q95cd_be_t24, quran_9_5_scope__contextual_defensive, base_extractiveness, 24, 0.22).
narrative_ontology:measurement(q95cd_be_t30, quran_9_5_scope__contextual_defensive, base_extractiveness, 30, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(q95cd_su_t0, quran_9_5_scope__contextual_defensive, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(q95cd_su_t6, quran_9_5_scope__contextual_defensive, suppression_requirement, 6, 0.22).
narrative_ontology:measurement(q95cd_su_t12, quran_9_5_scope__contextual_defensive, suppression_requirement, 12, 0.3).
narrative_ontology:measurement(q95cd_su_t18, quran_9_5_scope__contextual_defensive, suppression_requirement, 18, 0.34).
narrative_ontology:measurement(q95cd_su_t24, quran_9_5_scope__contextual_defensive, suppression_requirement, 24, 0.37).
narrative_ontology:measurement(q95cd_su_t30, quran_9_5_scope__contextual_defensive, suppression_requirement, 30, 0.36).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__contextual_defensive, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% Constraint family quran_9_5_scope decomposes the colloquial label 'the Sword Verse doctrine' into three epsilon-invariant readings. The label conflates structurally distinct claims: (1) an abrogationist universal-offense rule (massive fixed victim set, high extraction), (2) a contextual defensive-treaty rule (this file; narrow behavior-triggered victim set, low extraction), (3) a historicist dissolution of the verse into time-bound ethics (no standing legal constraint at all). The abrogating_universal reading is upstream: it dominated classical jurisprudential summaries for centuries and is cited as the plain reading by its holders; this reading's modern revival changes the legitimacy conditions of both siblings without resolving the dispute. Each family member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_9_5_scope__contextual_defensive, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
