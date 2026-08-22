% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanbali_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanbali_reading
 *   human_readable: Hanbali Textual-Restrictiveness Regime in Legal Derivation
 *   domain: religious/legal-theoretic/comparative-law
 *
 * SUMMARY:
 *   Within Hanbali jurisprudence, binding derivation runs through the Quran
 *   and rigorously authenticated hadith; analogy (qiyas) is confined to cases
 *   of clear textual silence; a weakly authenticated report is preferred over
 *   an analogical construction; and the blocking-of-means doctrine (sadd
 *   al-dhara'i) condemns practices that predictably lead to innovation before
 *   harm is shown. The arrangement solves a real coordination problem —
 *   convergent rulings, doctrinal continuity, a disciplined authentication
 *   science — while asymmetrically extracting from rationalist legal
 *   development, customary practice, and would-be innovators, whose warrant
 *   structures are delegitimized by the same machinery that coordinates the
 *   faithful. This file is ONE READING of the usul_al_fiqh_method kernel; the
 *   hanafi, maliki, and shafii readings are separate constraints with their
 *   own epsilon values, linked through the network. The claim and the metrics
 *   are independent authored facts: claimed_type records the structural
 *   judgment (both coordination and extraction present, actively enforced);
 *   the metrics record the arrangement's observed operation.
 *
 * KEY AGENTS:
 *   - - hanbali_textualist_establishment: Agenda-setter and dual-positioned collector (institutional/identity_locked) — administers authentication and fatwa; collects interpretive authority and enforcement mandate
 *   - - pious_laity_seeking_certainty: Beneficiary (moderate/constrained) — receives determinate rulings and boundary-clarity; absorbs rigidity indirectly
 *   - - rationalist_legal_theorists: Primary target (moderate/constrained) — bears delegitimation of analogical and preference-based method
 *   - - customary_practice_communities: Primary target (moderate/trapped) — inherited practice judged against the textual standard, no exit from their own customs
 *   - - would_be_innovators: Target (moderate/constrained) — proposals blocked pre-emptively by the means-blocking doctrine
 *   - - rival_madhhab_jurists: Excluded voice (institutional/mobile) — hold sibling readings; their warrants carry no force inside this frame
 *   - - modern_state_lawmakers: Excluded voice (institutional/mobile) — legislate with instruments the method disallows
 *   - - comparative_law_scholars: Analytical observer (analytical/analytical) — sees the full four-reading structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, 0.66).
domain_priors:suppression_score(usul_al_fiqh_method__hanbali_reading, 0.68).
domain_priors:theater_ratio(usul_al_fiqh_method__hanbali_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanbali_reading, "Hanbali Textual-Restrictiveness Regime in Legal Derivation").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanbali_reading, "religious/legal-theoretic/comparative-law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanbali_reading, 'bfe41f39-9468-40d5-a6e7-ea6c506beb45').
narrative_ontology:cs_kernel_codification('bfe41f39-9468-40d5-a6e7-ea6c506beb45', fixed_text).
narrative_ontology:cs_authority_grounding('bfe41f39-9468-40d5-a6e7-ea6c506beb45', lineage).
narrative_ontology:cs_interpretation_layer_present('bfe41f39-9468-40d5-a6e7-ea6c506beb45').
narrative_ontology:cs_reading_relation('bfe41f39-9468-40d5-a6e7-ea6c506beb45', usul_al_fiqh_method__hanafi_reading, forecloses).
narrative_ontology:cs_reading_relation('bfe41f39-9468-40d5-a6e7-ea6c506beb45', usul_al_fiqh_method__maliki_reading, forecloses).
narrative_ontology:cs_reading_relation('bfe41f39-9468-40d5-a6e7-ea6c506beb45', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('bfe41f39-9468-40d5-a6e7-ea6c506beb45', foundational, weak_hadith_preferred_over_qiyas).
narrative_ontology:cs_axiom_status(weak_hadith_preferred_over_qiyas, holdable).
narrative_ontology:cs_axiom_grounding('bfe41f39-9468-40d5-a6e7-ea6c506beb45', weak_hadith_preferred_over_qiyas, theological).
narrative_ontology:cs_axiom('bfe41f39-9468-40d5-a6e7-ea6c506beb45', foundational, innovation_means_blocking_obligatory).
narrative_ontology:cs_axiom_status(innovation_means_blocking_obligatory, holdable).
narrative_ontology:cs_axiom_grounding('bfe41f39-9468-40d5-a6e7-ea6c506beb45', innovation_means_blocking_obligatory, instrumental).
narrative_ontology:cs_reference_frame('bfe41f39-9468-40d5-a6e7-ea6c506beb45', authenticated_textual_sufficiency).
narrative_ontology:cs_drift_state('bfe41f39-9468-40d5-a6e7-ea6c506beb45', contemporary_global_salafi_revival, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('bfe41f39-9468-40d5-a6e7-ea6c506beb45', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_establishment).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, pious_laity_seeking_certainty).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, rationalist_legal_theorists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, customary_practice_communities).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, would_be_innovators).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, textual_supremacy_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, sunna_binding_authority).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, bid_a_prohibition_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trains jurists, transmits and grades hadith, issues fatwas, and staffs the virtue-enforcement and fatwa institutions of Hanbali-dominant polities. Its scholarly authority, livelihood, and self-conception are constituted by the restrictive method it administers; leaving the method would mean repudiating the tradition that defines its members' standing.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_establishment, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_establishment, beneficiary).

% Receives determinate halal/haram boundaries and stable worship practice from the method. Switching to a more permissive school carries social and familial cost, so most remain within the frame they benefit from, absorbing indirectly whatever rigidity the method imposes on their communities' evolving circumstances.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, pious_laity_seeking_certainty, beneficiary,
    moderate, biographical, constrained, global).

% Work with qiyas, juristic preference, and public-interest reasoning. Inside Hanbali jurisdiction their tools are delegitimized as gateways to innovation; they can practice within sibling schools or secular academies, but at reputational cost and loss of standing in the communities they trained to serve.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, rationalist_legal_theorists, payer,
    moderate, biographical, constrained, global).

% Hold local marriage forms, festival observances, shrine visitation, and transaction customs that the method evaluates against the textual standard. They cannot exit their own inherited practices without dissolving the community life those practices constitute; the blocking doctrine forecloses the intermediate adaptations they might otherwise negotiate.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, customary_practice_communities, payer,
    moderate, generational, trapped, regional).

% Propose new devotional forms, financial instruments, or institutional arrangements responsive to conditions the texts did not address. The blocking doctrine condemns the proposed means before any harm is shown, so their proposals die at the threshold rather than being tested; their alternatives are abandonment or exit to jurisdictions with more permissive warrant structures.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, would_be_innovators, payer,
    moderate, biographical, constrained, global).

% Hold the sibling readings of the same methodological kernel. Their warrant structures carry no force inside Hanbali adjudication; they argue their case in their own schools and in cross-school polemic, never inside the frame that excludes their tools by construction.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, rival_madhhab_jurists, excluded,
    institutional, generational, mobile, global).

% In Muslim-majority states they legislate family codes, commercial regulation, and public-interest statutes using instruments the restrictive method disallows. They route around the method rather than through it, and their objection — that governance needs tools the frame bars — registers inside the frame only as evidence of innovation.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, modern_state_lawmakers, excluded,
    institutional, generational, mobile, national).

% Study the four readings side by side, mapping how the same kernel produces different constraints under different source-orderings. They collect from no part of the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, comparative_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_establishment).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a single source hierarchy and authentication standard so jurists across generations and regions derive convergent rulings from revelation instead of idiosyncratic opinion, and maintains a shared, enforceable boundary between established practice and innovation.
% TRANSFER_FUNCTION: Moves interpretive jurisdiction and legal-development capacity from rationalist reasoning, customary practice, and novel proposals toward the authenticated textual corpus and its custodian class; moves doctrinal certainty and boundary-clarity to the laity.
% ABSENT_VOICES: Kalam-trained rationalist jurists whose tools the method bars, customary-law practitioners whose festivals and transactions stand judged, women and minorities subject to restrictively derived family and transaction rules, and modern legislators needing public-interest and custom-integration instruments the frame disallows. All stand outside the madhhab's adjudicative conversation; their objections enter only reframed as accusations of innovation or as external attack.
% DISAPPEARANCE_RATIONALE: Revelation and its study would persist, but the methodological regime would not: Hanbali-domain jurisprudence would reorganize around the sibling schools' more permissive warrant structures within a generation or two, thousands of rulings resting solely on textual restriction would lose their specific warrant, the innovation-policing apparatus would dissolve, and the customary and rationalist development currently suppressed would resume — the same rearrangement visible wherever Hanbali enforcement receded historically.
% FOUNDING_PROBLEM: After the Prophet's death, communities faced novel cases with no direct ruling, and early jurists diverged widely — free reasoning, weak transmissions, local Medinan custom. The Hanbali founding problem: bind legal derivation to authenticated revelation so that law tracks divine command rather than juristic preference, and pre-empt the accretion of unverifiable innovation — a commitment crystallized by Ahmad ibn Hanbal's stand for the uncreated Quran during the mihna, which made textual fidelity the school's defining identity.
% FOUNDING_PROBLEM_CORROBORATION: Sibling-school jurists attest the founding problem is live even while rejecting this reading's solution: Hanafi, Maliki, and Shafi'i usul literature all grapple with textual silence and innovation-control by different means. Academic historians of Islamic law, external to every beneficiary set, corroborate both the original juristic divergence and the problem's persistence. Contemporary fiqh academies convening on bioethics, finance, and digital life demonstrate that textual silence recurs every generation. Corroboration is therefore strong and independent of the Hanbali establishment.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanbali_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanbali_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanbali_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.66: the method transfers legal-development capacity from reason and custom to the textual custodian class — not wealth extraction but jurisdictional-epistemic extraction, substantial because the blocked territory (analogy beyond silence, public-interest departure, custom integration) is precisely where most legal development happens. Suppression is 0.68 as a raw, unscaled structural property: innovation-condemnation, means-blocking, and — in Hanbali-dominant polities — state-backed virtue enforcement. Theater is 0.32: hadith authentication and usul scholarship are genuinely functional, but a growing share of activity is ritualized policing of celebration, music, and devotional novelty whose connection to textual fidelity is nominal. Accessibility_collapse is 0.42: alternatives fully persist at the inter-madhhab level (three sibling schools operate openly) but collapse substantially within Hanbali jurisdiction once the method is accepted. Resistance is 0.55: fourteen centuries of rationalist, Sufi-inclined, and reformist pushback, plus sibling-school competition. The measurement series run on one shared seven-point grid (855–2024) so every tracked metric is authored at every examined time point. The 1500 dip reflects Ottoman Hanafi dominance shrinking Hanbali enforcement reach; the 1925 peak reflects the Saudi instantiation (virtue-enforcement committees, shrine demolitions, codified textualism); the slight 2024 easing reflects state entertainment liberalization while social enforcement persists.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the establishment seat the arrangement is sacred fidelity it stewards at real cost — the engine should read a coordinated, low-extraction structure from that position. From the rationalist and customary seats the same structure is enforced closure of method-space: their tools and practices are condemned by machinery they cannot appeal to. From the laity seat it is a certainty-good with diffuse, indirect costs. The engine derives these per-seat classifications from the power, exit, and beneficiary/victim data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The establishment is a declared beneficiary with identity_locked exit: its d sits near the beneficiary end, and its lock keeps it invested in maintenance rather than revision. The laity are beneficiaries with constrained exit — low d, mildly dampened by the indirect rigidity they absorb. Rationalist theorists are declared victims with constrained exit — high d, amplified by the reputational cost of exit. Customary communities are victims who are trapped: their own inherited practice is the object under evaluation, placing them nearest the full-target end. Would-be innovators are victims with constrained exit — high d, since the blocking doctrine fires before any alternative venue opens. The excluded seats (rival jurists, state lawmakers) fall to canonical fallback: they are outside the frame rather than positioned within it.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy resolution is declared: the founding problem — binding derivation under recurring textual silence — is live, re-triggered by every technological and social novelty. The tangled_rope classification guards against both mislabels. Labeling the arrangement a pure rope would conceal the asymmetric extraction running through the same structure that coordinates: the authentication discipline that serves the laity is the identical machinery that delegitimizes rationalist and customary development. Labeling it a snare would erase the genuine coordination function that even rival schools implicitly rely on — shared authentication standards and source-hierarchy discipline — and would misread conviction-driven adherence as mere coercion. The hybrid category holds both facts without letting either cover story absorb the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the usul_al_fiqh_method kernel: which structural features are constitutive of the hanbali_reading specifically, versus furniture shared by the whole kernel?',
    'Cross-reading comparison across the four files: features invariant across all readings belong to the kernel; features unique to this ordering (weak-hadith-over-qiyas priority, obligatory means-blocking) are reading-specific.',
    'Sibling readings instantiate different constraints with different epsilon: the hanafi reading lowers extraction on rationalist development; the maliki reading raises accommodation of custom. Verdicts computed for this file must not be generalized to the kernel or back-propagated onto siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one of four readings; disagreement located in source-hierarchy ordering and non-textual warrant admissibility.').

omega_variable(
    revelation_constraint_vs_constructed_regime,
    'Is the restrictive method a response to a genuine epistemic constraint (revelation''s authority over law) or a constructed arrangement whose primary effect is concentrating interpretive jurisdiction in the textualist establishment?',
    'Compare ruling-convergence rates and jurisdictional concentration across periods and places where the method operates with versus without enforcement backing; test whether fidelity outcomes track enforcement capacity or persuasive conviction.',
    'If enforcement-dependent, the arrangement behaves as enforced extraction riding a coordination story, pressuring classification toward the snare end; if persuasion-sufficient, the coordination reading strengthens and measured extraction reads closer to inherent coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_constraint_vs_constructed_regime, empirical, 'Natural-law-shaped versus constructed ambiguity in the method''s authority.').

omega_variable(
    suppression_internalization_split,
    'Is the measured suppression structural (hisba institutions, state virtue enforcement, social sanction) or internalized (conviction that innovation is sin, identity fusion with textualist belonging)?',
    'Observe adherence trajectories where enforcement machinery withdrew — post-liberalization policy shifts in Hanbali-dominant states, diaspora communities outside enforcement reach: persistence of innovation-policing without machinery indicates internalization.',
    'If largely internalized, suppression outlasts its enforcement substrate and effective suppression exceeds the structural measure; the omega feeds the structural-versus-internalized decomposition the scalar suppression metric cannot express.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized suppression mechanism split.').

omega_variable(
    weak_hadith_reliability_tradeoff,
    'Does preferring weak hadith over qiyas preserve proximity to revelation (transmission beats inference) or import unreliable content into law (weak chains admit fabrication and error)?',
    'Hadith-criticism correlation studies and outcome audits comparing rulings resting solely on weak reports against analogous derivations on matching fact patterns.',
    'If weak-report rulings show high reversal or error rates, the priority rule degrades the coordination output and excess extraction rises; if stable, the rule functions as low-cost textual anchoring and the rope-side reading gains support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(weak_hadith_reliability_tradeoff, empirical, 'Epistemic quality of the weak-hadith-over-qiyas priority rule.').

omega_variable(
    sadd_dharai_scope_ambiguity,
    'Does sadd al-dhara''i block only means that probably lead to prohibited ends, or does it operate as a general conservative veto against any novel practice?',
    'Code the historical and contemporary fatwa corpus: frequency of means-blocking invocations with quantified harm-probability versus bare-novelty invocations.',
    'Narrow scope confines extraction to genuine harm-prevention; broad scope converts the doctrine into a standing barrier on all legal-development capacity, sharply raising effective extraction on would-be innovators and customary adapters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sadd_dharai_scope_ambiguity, conceptual, 'Scope ambiguity of the innovation-blocking doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanbali_reading, 855, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_hanbali_tr_t855, usul_al_fiqh_method__hanbali_reading, theater_ratio, 855, 0.12).
narrative_ontology:measurement_basis(usul_hanbali_tr_t855, observed).
narrative_ontology:measurement(usul_hanbali_tr_t1100, usul_al_fiqh_method__hanbali_reading, theater_ratio, 1100, 0.16).
narrative_ontology:measurement_basis(usul_hanbali_tr_t1100, observed).
narrative_ontology:measurement(usul_hanbali_tr_t1328, usul_al_fiqh_method__hanbali_reading, theater_ratio, 1328, 0.22).
narrative_ontology:measurement_basis(usul_hanbali_tr_t1328, observed).
narrative_ontology:measurement(usul_hanbali_tr_t1500, usul_al_fiqh_method__hanbali_reading, theater_ratio, 1500, 0.24).
narrative_ontology:measurement_basis(usul_hanbali_tr_t1500, observed).
narrative_ontology:measurement(usul_hanbali_tr_t1750, usul_al_fiqh_method__hanbali_reading, theater_ratio, 1750, 0.28).
narrative_ontology:measurement_basis(usul_hanbali_tr_t1750, observed).
narrative_ontology:measurement(usul_hanbali_tr_t1925, usul_al_fiqh_method__hanbali_reading, theater_ratio, 1925, 0.34).
narrative_ontology:measurement_basis(usul_hanbali_tr_t1925, observed).
narrative_ontology:measurement(usul_hanbali_tr_t2024, usul_al_fiqh_method__hanbali_reading, theater_ratio, 2024, 0.32).
narrative_ontology:measurement_basis(usul_hanbali_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(usul_hanbali_be_t855, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 855, 0.44).
narrative_ontology:measurement_basis(usul_hanbali_be_t855, observed).
narrative_ontology:measurement(usul_hanbali_be_t1100, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 1100, 0.48).
narrative_ontology:measurement_basis(usul_hanbali_be_t1100, observed).
narrative_ontology:measurement(usul_hanbali_be_t1328, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 1328, 0.56).
narrative_ontology:measurement_basis(usul_hanbali_be_t1328, observed).
narrative_ontology:measurement(usul_hanbali_be_t1500, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 1500, 0.53).
narrative_ontology:measurement_basis(usul_hanbali_be_t1500, observed).
narrative_ontology:measurement(usul_hanbali_be_t1750, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 1750, 0.6).
narrative_ontology:measurement_basis(usul_hanbali_be_t1750, observed).
narrative_ontology:measurement(usul_hanbali_be_t1925, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 1925, 0.7).
narrative_ontology:measurement_basis(usul_hanbali_be_t1925, observed).
narrative_ontology:measurement(usul_hanbali_be_t2024, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 2024, 0.66).
narrative_ontology:measurement_basis(usul_hanbali_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(usul_hanbali_su_t855, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 855, 0.34).
narrative_ontology:measurement_basis(usul_hanbali_su_t855, observed).
narrative_ontology:measurement(usul_hanbali_su_t1100, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 1100, 0.38).
narrative_ontology:measurement_basis(usul_hanbali_su_t1100, observed).
narrative_ontology:measurement(usul_hanbali_su_t1328, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 1328, 0.52).
narrative_ontology:measurement_basis(usul_hanbali_su_t1328, observed).
narrative_ontology:measurement(usul_hanbali_su_t1500, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 1500, 0.48).
narrative_ontology:measurement_basis(usul_hanbali_su_t1500, observed).
narrative_ontology:measurement(usul_hanbali_su_t1750, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 1750, 0.6).
narrative_ontology:measurement_basis(usul_hanbali_su_t1750, observed).
narrative_ontology:measurement(usul_hanbali_su_t1925, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 1925, 0.78).
narrative_ontology:measurement_basis(usul_hanbali_su_t1925, observed).
narrative_ontology:measurement(usul_hanbali_su_t2024, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 2024, 0.68).
narrative_ontology:measurement_basis(usul_hanbali_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanbali_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, shafii_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'usul al-fiqh': the label covers four structurally distinct source-ordering regimes, each with its own epsilon, beneficiary/victim structure, and classification. This file is the hanbali member (highest textual restrictiveness, lowest analogical/customary scope). The upstream shared kernel (authentication discipline, source-hierarchy concept) influences all four; the siblings are linked here so contamination and coupling analysis treats the family as one decomposed unit rather than one undifferentiated constraint. Per the epsilon-invariance principle, no single file averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
