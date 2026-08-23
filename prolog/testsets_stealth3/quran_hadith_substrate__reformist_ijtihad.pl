% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__reformist_ijtihad
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__reformist_ijtihad, []).

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
 *   constraint_id: quran_hadith_substrate__reformist_ijtihad
 *   human_readable: Reformist Contextual Ijtihad Mandate
 *   domain: religious/legal-theoretical
 *
 * SUMMARY:
 *   This file instantiates ONE reading — reformist_ijtihad — of the contested
 *   kernel quran_hadith_substrate (the Quran-and-hadith corpus as the ground
 *   of Islamic legal authority). In jurisdictions and institutions where
 *   reformist currents hold backing (progressive seminaries, certain state
 *   councils and muftiates, transnational academic networks), a
 *   methodological mandate operates: when an inherited madhhab ruling
 *   conflicts with contemporary ethical commitments, human-rights norms, or
 *   verified public interest (maslaha), the jurist must rederive the ruling
 *   contextually, giving the Quran's ethical trajectory priority over
 *   literalist application of hadith that cuts against it. The mandate
 *   coordinates a living jurisprudence that keeps conscience-stricken
 *   believers inside the tradition; the same operation strips interpretive
 *   authority from the ulama class and madhhab institutions whose legitimacy
 *   rests on settled consensus — hence both a real coordination function and
 *   a real, asymmetrically borne cost. Per the epsilon-referent rule, epsilon
 *   here assesses THIS mandate arrangement as it actually operates in backed
 *   contexts, read by this reading's own lights — not the traditionalist
 *   arrangement (a separate file) and not the reading's endorsed ideal.
 *   Sibling readings traditionalist_taqlid and state_hybrid instantiate the
 *   same kernel differently and are linked via network edges, not folded into
 *   this story. KEY AGENTS (by structural relationship): - reformist_jurists:
 *   agenda-setting interpreter class (institutional/mobile) — issues the
 *   mandate, staffs the councils, collects the displaced authority -
 *   progressive_muslims: primary beneficiary constituency
 *   (moderate/constrained) — purchases continued observance with the mandate
 *   - muslim_women: principal doctrinal beneficiary (organized/constrained) —
 *   concrete family-law gains where adopted - lgbtq_muslims: acute
 *   beneficiary seat (powerless/identity_locked) — first institutional
 *   opening, double identity bind - religious_minorities: beneficiary via
 *   state-law effects (powerless/constrained) — equal-citizenship promise -
 *   traditional_ulama: primary target (institutional/identity_locked) —
 *   authority demoted, identity fused with the displaced method -
 *   madhhab_institutions: secondary target (institutional/constrained) — the
 *   doctrine set constituting their function shrinks with each override -
 *   salafi_literalist_scholars: excluded objector (organized/mobile) — barred
 *   from the codifying rooms, preaches from parallel infrastructure -
 *   islamic_studies_academics: analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__reformist_ijtihad, 0.44).
domain_priors:suppression_score(quran_hadith_substrate__reformist_ijtihad, 0.42).
domain_priors:theater_ratio(quran_hadith_substrate__reformist_ijtihad, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, extractiveness, 0.44).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__reformist_ijtihad, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__reformist_ijtihad, "Reformist Contextual Ijtihad Mandate").
narrative_ontology:topic_domain(quran_hadith_substrate__reformist_ijtihad, "religious/legal-theoretical").

domain_priors:requires_active_enforcement(quran_hadith_substrate__reformist_ijtihad).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__reformist_ijtihad, '5f4f0385-e5ed-435f-82dd-d5bd427683ff').
narrative_ontology:cs_kernel_codification('5f4f0385-e5ed-435f-82dd-d5bd427683ff', fixed_text).
narrative_ontology:cs_authority_grounding('5f4f0385-e5ed-435f-82dd-d5bd427683ff', expertise).
narrative_ontology:cs_interpretation_layer_present('5f4f0385-e5ed-435f-82dd-d5bd427683ff').
narrative_ontology:cs_reading_relation('5f4f0385-e5ed-435f-82dd-d5bd427683ff', quran_hadith_substrate__traditionalist_taqlid, forecloses).
narrative_ontology:cs_reading_relation('5f4f0385-e5ed-435f-82dd-d5bd427683ff', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('5f4f0385-e5ed-435f-82dd-d5bd427683ff', foundational, ethical_trajectory_primacy_over_literalism).
narrative_ontology:cs_axiom_status(ethical_trajectory_primacy_over_literalism, holdable).
narrative_ontology:cs_axiom_grounding('5f4f0385-e5ed-435f-82dd-d5bd427683ff', ethical_trajectory_primacy_over_literalism, theological).
narrative_ontology:cs_axiom('5f4f0385-e5ed-435f-82dd-d5bd427683ff', foundational, maslaha_override_of_conflicting_classical_rulings).
narrative_ontology:cs_axiom_status(maslaha_override_of_conflicting_classical_rulings, holdable).
narrative_ontology:cs_axiom_grounding('5f4f0385-e5ed-435f-82dd-d5bd427683ff', maslaha_override_of_conflicting_classical_rulings, instrumental).
narrative_ontology:cs_reference_frame('5f4f0385-e5ed-435f-82dd-d5bd427683ff', quran_ethical_trajectory_frame).
narrative_ontology:cs_drift_state('5f4f0385-e5ed-435f-82dd-d5bd427683ff', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5f4f0385-e5ed-435f-82dd-d5bd427683ff', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, progressive_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, muslim_women).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, lgbtq_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, religious_minorities).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, reformist_jurists).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, traditional_ulama).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, madhhab_institutions).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__reformist_ijtihad, maqasid_al_sharia_doctrine).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__reformist_ijtihad, double_movement_hermeneutics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach and publish the contextual method, staff reformist councils and seminaries, and issue rulings that override inherited positions when these conflict with contemporary ethical commitments. The mandate is their professional warrant: chairs, journals, consultancies, and council seats exist because the method is adopted. Where backing erodes, their institutional positions erode with it; they can move between academia, media, and transnational networks.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, reformist_jurists, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__reformist_ijtihad, reformist_jurists, beneficiary).

% Educated urban believers whose ethical commitments collide with inherited rulings on gender, apostasy, and pluralism. The mandate gives them rulings they can follow without splitting conscience or leaving the community. Leaving the tradition entirely remains possible but carries family, community, and identity costs most are unwilling to pay.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, progressive_muslims, beneficiary,
    moderate, biographical, constrained, global).

% Advocacy networks and ordinary women pressing for rulings on marriage, divorce, testimony, and inheritance compatible with equal standing. Contextual rederivation has delivered concrete changes where adopted — reformed family codes, expanded testimony and divorce rights. Gains arrive jurisdiction by jurisdiction and can be reversed where traditionalist coalitions retake councils.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, muslim_women, beneficiary,
    organized, generational, constrained, global).

% Believers seeking to remain observant while living outside inherited sexual-ethics rulings. Contextual method offers the first institutional opening in most of the tradition's history. They carry a double bind: exiting the faith severs community and family; concealing orientation severs integrity. Neither exit is cheap, so institutional openings matter disproportionately to them.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, lgbtq_muslims, beneficiary,
    powerless, biographical, identity_locked, global).

% Non-Muslim citizens living under legal orders that draw on Islamic law. Inherited frameworks assign differentiated civic status; human-rights-norm rederivation promises equal citizenship. Their practical condition depends on which interpretive method governs the state law they live under, and they hold little direct leverage over that choice.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, religious_minorities, beneficiary,
    powerless, generational, constrained, regional).

% Madhhab-trained scholars whose standing rests on transmission chains, mastery of inherited doctrine, and consensus. Each mandated contextual override demotes parts of that inheritance to historically contingent artifacts. Their credentials, posts, and self-understanding were formed inside the method now being displaced; stepping outside it would repudiate the formation itself, so they fight the mandate from within their own institutions instead.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditional_ulama, payer,
    institutional, generational, identity_locked, global).

% Seminaries, endowments, fatwa councils, and schools organized around settled school doctrine. Every adopted contextual override shrinks the doctrine set that constitutes their function. Adapting by internalizing the contextual method preserves the shell but transforms what the institution is; resisting preserves identity at the cost of relevance.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, madhhab_institutions, payer,
    institutional, generational, constrained, global).

% Scholars of literalist hadith method who hold no seat in reformist councils, state reform processes, or the academic forums where the mandate is elaborated. They regard subordinating hadith to an inferred ethical trajectory as an attack on the Prophet's authority itself and preach against the mandate from parallel pulpits, satellite channels, and online networks. Their objection never enters the rooms where the method is codified.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, salafi_literalist_scholars, excluded,
    organized, generational, mobile, global).

% Researchers in Islamic studies, law, and anthropology who document the contest between interpretive methods, trace the genealogy of maqasid reasoning, and map which jurisdictions and institutions adopt which method. They take no side in the authority contest and neither gain nor lose rulings from its outcome.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, islamic_studies_academics, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__reformist_ijtihad, reformist_jurists).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__reformist_ijtihad, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single, principled method for deriving actionable rulings from a fixed textual corpus under radically changed social conditions: it tells jurists when inherited positions stand and when they must be rederived, keeps dispersed reformist institutions producing mutually recognizable rulings, and gives lay believers a stable account of why their conscience and their law no longer conflict.
% TRANSFER_FUNCTION: Moves interpretive authority — and the status, posts, and gatekeeping power attached to it — from madhhab-trained ulama and their institutions to reformist jurists, academic networks, and the councils that adopt contextual method; downstream it moves doctrinal outcomes (family-code provisions, testimony and divorce rights, minority-status rules) from inherited positions toward rights-compatible ones.
% ABSENT_VOICES: Literalist hadith scholars and lay traditionalists are absent from the councils, seminaries, and conferences where the mandate is elaborated; so are ordinary believers in traditionalist-majority regions who never encounter reformist discourse and whose tacit assent to inherited practice is counted neither for nor against. Present, they would press the bindingness of the sunnah and warn that an 'ethical trajectory' inferred by fallible readers licenses arbitrary revision.
% DISAPPEARANCE_RATIONALE: If the mandate vanished overnight, reformist institutions would lose their methodological warrant and much of their reason to exist; believers currently accommodated by contextual rulings would face the conscience-ruling conflict anew, accelerating exit or quiet nonobservance; traditional authorities would regain uncontested interpretive monopoly; and jurisdictions that reformed family law on maqasid grounds would face pressure to revert. The arrangement is load-bearing for a large population's continued observance.
% FOUNDING_PROBLEM: Nineteenth- and twentieth-century encounters with colonial law, nation-states, and universal rights discourses exposed classical rulings — on women's standing, apostasy, religious minorities, punishment — as increasingly indefensible to educated believers, producing crises of conscience, hypocrisy, or exit. The arrangement was built to reconcile revealed texts with changed moral circumstances while preserving the tradition's authority and keeping its adherents inside it.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist scholars themselves attest the crisis — their polemics concede that believers are leaving or doubting over precisely these rulings, even while disputing the contextual remedy. Academic Islamic studies and demography document the conscience-crisis and quiet disaffection from outside all confessional camps, and conservative states' own piecemeal family-law reforms implicitly concede the problem. Corroboration therefore exists well outside the reformist beneficiary set.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__reformist_ijtihad, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__reformist_ijtihad, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__reformist_ijtihad, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_hadith_substrate__reformist_ijtihad, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__reformist_ijtihad, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__reformist_ijtihad_tests).
:- end_tests(quran_hadith_substrate__reformist_ijtihad_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction sits at 0.44: the mandate transfers real assets — interpretive authority, institutional posts, gatekeeping power — but transfers status and jurisdiction rather than wealth, operates only where reformists hold backing, and leaves the losing side its institutions and audiences. Suppression at 0.42 is the machinery needed to hold the mandate open against traditionalist counter-mobilization and to keep literalist method marginal inside backed institutions; it is predominantly structural (credential gatekeeping, council composition, state backing) with an internalized component (self-censorship among traditionally trained scholars working in reformist spaces) — the composition ambiguity is carried by the suppression_mechanism_composition omega. Theater at 0.31: most reformist output is functional — rulings actually change, family codes actually amend — but a growing share is rhetorical invocation of maslaha by institutions that change little, plus conference-scale performativity. Accessibility_collapse at 0.35: understanding the mandate does not close alternatives — taqlid remains fully available and is the majority practice worldwide; the mandate competes rather than extinguishes. Resistance at 0.68: sustained heresy accusations, institutional competition, and state-level reversal campaigns. The claimed_type (tangled_rope) is authored independently of these metrics: the mandate genuinely coordinates (it solves the tradition-modernity reconciliation problem for millions) and genuinely extracts (it dispossesses a specific authority class through the same structure that coordinates everyone else). The measurement series share one seven-point grid (1900-2025); the extractiveness series shows a full rise-dip-rise cycle driven by exogenous political waves (colonial-era reformism, mid-century institutionalization, the 1970s-80s Islamist resurgence squeezing reformist authority, the post-2000 progressive rebuild) — the oscillation is a side effect of the broader political pendulum, not an intermittent-reinforcement mechanism. Base properties are measured at interval end (2025), on the rising phase. fixing_cost is prohibitive: no seat can remove the mandate cheaply — traditionalists must win the doctrinal contest outright, reformists would dissolve their own warrant, and the underlying conscience-crisis regenerates demand for some reconciliation method either way.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute differently from the same structure. From the traditional-ulama seat the mandate is dispossession: a lifetime formation in transmitted doctrine is retroactively demoted to contingency by people with thinner credentials, and the loss is total because their authority, income, and self-concept are fused with the displaced method — identity-lock of the professional-institutional kind, where breaking the frame means repudiating the self; if that frame broke, the classification of their seat would shift from trapped-target toward mobile-adaptor. From the lgbtq_muslim seat the same mandate is the only door that has ever opened, and their own identity-lock (faith and orientation each unbearable to abandon) makes the stakes existential rather than reputational. From the agenda-setter seat it is vocation: the mandate is what reformist jurists are for. The engine computes these divergent per-seat classifications from power, exit, and role data; this story does not adjudicate which seat's experience is 'the' constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive d toward zero for progressive_muslims, muslim_women, lgbtq_muslims, and religious_minorities — the mandate subsidizes their continued observance and civic standing; among them, identity_locked lgbtq_muslims and powerless religious_minorities sit nearest the full-beneficiary end because exit cannot arbitrage their position. Victim declarations drive traditional_ulama and madhhab_institutions toward the full-target end: they bear the entire authority transfer, and the ulama's identity_lock removes the exit modulation that would otherwise damp their effective burden. reformist_jurists straddle: they administer the mandate and collect what it displaces, so their d sits near the beneficiary pole despite carrying administrative load. Spatial scope is effectively global (the mandate travels through transnational seminaries, media, and diaspora institutions), which modestly amplifies effective extraction on the target side by making verification of 'genuine maslaha' harder. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already separate the seats correctly, and the shared power atoms (three institutional seats with three different relationships) are distinguished by role and exit rather than by override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling revealed text with changed moral circumstance — is live, so no mandatrophy is declared and no sunset applies. The tangled_rope classification earns its keep in both directions: reading the mandate as pure rope would erase the real, concentrated dispossession of the ulama class that the same structure accomplishes; reading it as a snare would erase the genuine coordination delivered — millions of believers retain an observant identity they would otherwise forfeit, and concrete rights-compatible rulings exist where the method is adopted. If a stable synthesis ever emerged (contextual method becoming uncontested common law), the mandate's extractive edge would fade and the constraint would decay toward rope or scaffold; the counter_mobilization_vulnerability omega tracks whether the opposite decay — into theatrical maintenance inside shrinking enclaves — is underway instead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the quran_hadith_substrate kernel; how would instantiating a sibling reading change the structural facts?',
    'Compare the compiled stories for traditionalist_taqlid and state_hybrid: victim and beneficiary sets, enforcement requirements, and epsilon should differ systematically along the ijma-bindingness and authority-locus axes.',
    'Under traditionalist_taqlid the victim set expands to every would-be independent reasoner and epsilon rises; under state_hybrid the mandate dissolves into selective sovereign adoption and this story''s enforcement surface largely disappears.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one of three readings of a shared textual kernel; sibling readings redistribute beneficiaries, victims, and enforcement.').

omega_variable(
    elite_recapture_question,
    'Does the interpretive authority transferred by the mandate reach the affected classes (women, minorities, queer believers), or is it recaptured by a new scholarly elite operating contextual method?',
    'Track who occupies decision seats in reformist councils and seminaries over time, and whether rulings adopted under the mandate track beneficiary-class priorities or scholar-class priorities.',
    'If recaptured, the mandate''s coordination benefit narrows to elite circulation, the burden on the traditional class rises without compensating delivery, and the beneficiary declarations overstate the subsidy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_recapture_question, empirical, 'Whether the authority transfer delivers to intended beneficiaries or is recaptured by a replacement interpretive elite.').

omega_variable(
    counter_mobilization_vulnerability,
    'Will institutional backing for the mandate survive traditionalist counter-mobilization, or is the current backing a transient political window?',
    'Observe council compositions, state appointments, and seminary enrollments across coming political cycles in the backing jurisdictions; watch for reversal campaigns succeeding.',
    'If backing collapses, the mandate loses enforcement, its effective burden on the traditional class falls toward zero in most jurisdictions, and surviving enclaves drift toward theatrical maintenance of a method no longer operative — a decay path the theater_ratio series would show rising sharply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_mobilization_vulnerability, empirical, 'Persistence of the institutional backing on which the mandate''s operation depends.').

omega_variable(
    suppression_mechanism_composition,
    'How much of the measured suppression is structural (credential gatekeeping, council composition, state backing) versus internalized (self-censorship by traditionally trained scholars inside reformist spaces)?',
    'Post-backing suppression trajectory: if literalist voices resurface quickly where state backing withdraws, suppression was structural; if they stay muted, an internalized component persists.',
    'If substantially internalized, the mandate''s suppressive force outlives its enforcement infrastructure and would not vanish with institutional reversal — raising the persistence floor of the measured suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Structural versus internalized composition of the mandate''s suppressive force.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel best framed as the TEXTS (the Quran-and-hadith corpus as fixed_text) or as the COMMUNITY''S AUTHORITY STRUCTURE that adjudicates them? The two framings support different commitment-system classifications.',
    'Run both framings through the commitment-system battery: a text-first framing yields fixed_text codification with expertise adjudication; an authority-structure framing yields distributed codification with contested adjudication.',
    'Under the authority-structure framing, this reading loses its fixed-text anchor, its drift profile shifts from repudiation_pressure toward codification_collapse, and the foreclosure relation to the traditionalist sibling weakens (competing adjudicators rather than contradictory premises).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Framing under-determination: text-kernel versus authority-structure-kernel framings of the same commitment system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__reformist_ijtihad, 1900, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quran_hadith_reform_ijtihad_tr_t1900, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 1900, 0.12).
narrative_ontology:measurement_basis(quran_hadith_reform_ijtihad_tr_t1900, observed).
narrative_ontology:measurement(quran_hadith_reform_ijtihad_tr_t1925, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 1925, 0.16).
narrative_ontology:measurement_basis(quran_hadith_reform_ijtihad_tr_t1925, observed).
narrative_ontology:measurement(quran_hadith_reform_ijtihad_tr_t1950, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 1950, 0.18).
narrative_ontology:measurement_basis(quran_hadith_reform_ijtihad_tr_t1950, observed).
narrative_ontology:measurement(quran_hadith_reform_ijtihad_tr_t1975, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 1975, 0.22).
narrative_ontology:measurement_basis(quran_hadith_reform_ijtihad_tr_t1975, observed).
narrative_ontology:measurement(quran_hadith_reform_ijtihad_tr_t2000, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2000, 0.24).
narrative_ontology:measurement_basis(quran_hadith_reform_ijtihad_tr_t2000, observed).
narrative_ontology:measurement(quran_hadith_reform_ijtihad_tr_t2012, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2012, 0.29).
narrative_ontology:measurement_basis(quran_hadith_reform_ijtihad_tr_t2012, observed).
narrative_ontology:measurement(quran_hadith_reform_ijtihad_tr_t2025, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2025, 0.31).
narrative_ontology:measurement_basis(quran_hadith_reform_ijtihad_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(quran_hadith_reform_ijtihad_be_t1900, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 1900, 0.22).
narrative_ontology:measurement_basis(quran_hadith_reform_ijtihad_be_t1900, observed).
narrative_ontology:measurement(quran_hadith_reform_ijtihad_be_t1925, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 1925, 0.28).
narrative_ontology:measurement_basis(quran_hadith_reform_ijtihad_be_t1925, observed).
narrative_ontology:measurement(quran_hadith_reform_ijtihad_be_t1950, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 1950, 0.33).
narrative_ontology:measurement_basis(quran_hadith_reform_ijtihad_be_t1950, observed).
narrative_ontology:measurement(quran_hadith_reform_ijtihad_be_t1975, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 1975, 0.27).
narrative_ontology:measurement_basis(quran_hadith_reform_ijtihad_be_t1975, observed).
narrative_ontology:measurement(quran_hadith_reform_ijtihad_be_t2000, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2000, 0.36).
narrative_ontology:measurement_basis(quran_hadith_reform_ijtihad_be_t2000, observed).
narrative_ontology:measurement(quran_hadith_reform_ijtihad_be_t2012, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2012, 0.41).
narrative_ontology:measurement_basis(quran_hadith_reform_ijtihad_be_t2012, observed).
narrative_ontology:measurement(quran_hadith_reform_ijtihad_be_t2025, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2025, 0.44).
narrative_ontology:measurement_basis(quran_hadith_reform_ijtihad_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(quran_hadith_reform_ijtihad_su_t1900, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 1900, 0.18).
narrative_ontology:measurement_basis(quran_hadith_reform_ijtihad_su_t1900, observed).
narrative_ontology:measurement(quran_hadith_reform_ijtihad_su_t1925, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 1925, 0.26).
narrative_ontology:measurement_basis(quran_hadith_reform_ijtihad_su_t1925, observed).
narrative_ontology:measurement(quran_hadith_reform_ijtihad_su_t1950, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement_basis(quran_hadith_reform_ijtihad_su_t1950, observed).
narrative_ontology:measurement(quran_hadith_reform_ijtihad_su_t1975, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 1975, 0.38).
narrative_ontology:measurement_basis(quran_hadith_reform_ijtihad_su_t1975, observed).
narrative_ontology:measurement(quran_hadith_reform_ijtihad_su_t2000, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2000, 0.34).
narrative_ontology:measurement_basis(quran_hadith_reform_ijtihad_su_t2000, observed).
narrative_ontology:measurement(quran_hadith_reform_ijtihad_su_t2012, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2012, 0.39).
narrative_ontology:measurement_basis(quran_hadith_reform_ijtihad_su_t2012, observed).
narrative_ontology:measurement(quran_hadith_reform_ijtihad_su_t2025, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2025, 0.42).
narrative_ontology:measurement_basis(quran_hadith_reform_ijtihad_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__reformist_ijtihad, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, state_hybrid).

% DUAL FORMULATION NOTE:
% The colloquial label 'Islamic law's textual foundations' conflates three structurally distinct arrangements that share one kernel: mandated contextual rederivation (this file), obligatory adherence to settled madhhab consensus (traditionalist_taqlid), and sovereign partition of the corpus across legal domains (state_hybrid). Their epsilon values differ because the referent arrangements differ: this reading's epsilon measures the authority transfer the mandate performs in backed contexts; the traditionalist reading's measures the closure of independent reasoning; the state-hybrid reading's measures selective doctrinal application. The readings are linked, not merged: reformist scholarship supplies doctrinal material to state hybrids (influences), and the reformist mandate's premise contradicts taqlid's bindingness within any single juristic framework (forecloses).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
