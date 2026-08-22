% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__dignity_reading, []).

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
 *   constraint_id: speech_harm_boundary__dignity_reading
 *   human_readable: Dignity-Subordinate Speech Protection (Categorical Personhood-Denial Exclusions)
 *   domain: constitutional/political philosophy/communication ethics
 *
 * SUMMARY:
 *   This story instantiates the dignity reading of the speech_harm_boundary
 *   kernel: a constitutional arrangement in which speech protection is
 *   expressly subordinate to human dignity and personhood-denying speech sits
 *   outside protection altogether, enforced through categorical criminal
 *   exclusions rather than case-by-case balancing. The arrangement
 *   coordinates civic equality (a genuine collective function: it removes
 *   group status from discursive combat and interrupts documented
 *   dehumanization cascades) while extracting heavily from speakers (criminal
 *   liability, chilling effects, definitional uncertainty) and accruing
 *   definitional power to the enforcement apparatus. The epsilon referent is
 *   the dignity-subordination arrangement itself, assessed by this reading's
 *   own lights: the reading endorses the speaker burden as justified, but
 *   epsilon measures the burden, not its justification — the design
 *   deliberately imposes heavy restriction on speakers of identity-harm, and
 *   that is what the 0.72 records. Sibling readings (absolutist_reading,
 *   harm_balancing_reading) instantiate different constraints with different
 *   epsilon, victim sets, and classifications; they are separate stories
 *   linked through the network, not folded into this one.
 *
 * KEY AGENTS:
 *   - - dignity_protected_minority_groups: Primary beneficiary (organized/constrained) — receives the dignity shield
 *   - - holocaust_memory_communities: Secondary beneficiary (organized/constrained) — receives enforceable historical recognition
 *   - - identity_harm_speakers: Primary target (powerless/constrained) — bears criminal liability for excluded speech
 *   - - heterodox_public_commentators: Secondary target (moderate/mobile) — bears chilling effects near the boundary
 *   - - minority_faith_critics: Dual-positioned target (moderate/constrained) — nominally protected, empirically over-prosecuted
 *   - - dignity_law_enforcers: Agenda setter (institutional/constrained) — defines and widens the categories, accrues authority
 *   - - civil_liberties_organizations: Excluded voice (organized/trapped) — contests the margins, not the axiom
 *   - - offshore_publishing_platforms: Excluded arbiter (powerful/arbitrage) — hosts what the arrangement excludes
 *   - - comparative_constitutional_scholars: Analytical observer (analytical/analytical) — sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, 0.72).
domain_priors:suppression_score(speech_harm_boundary__dignity_reading, 0.66).
domain_priors:theater_ratio(speech_harm_boundary__dignity_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__dignity_reading, "Dignity-Subordinate Speech Protection (Categorical Personhood-Denial Exclusions)").
narrative_ontology:topic_domain(speech_harm_boundary__dignity_reading, "constitutional/political philosophy/communication ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__dignity_reading, '7da9c44a-bfb7-41cf-b03f-44583ead467b').
narrative_ontology:cs_kernel_codification('7da9c44a-bfb7-41cf-b03f-44583ead467b', fixed_text).
narrative_ontology:cs_authority_grounding('7da9c44a-bfb7-41cf-b03f-44583ead467b', lineage).
narrative_ontology:cs_interpretation_layer_present('7da9c44a-bfb7-41cf-b03f-44583ead467b').
narrative_ontology:cs_reading_relation('7da9c44a-bfb7-41cf-b03f-44583ead467b', speech_harm_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('7da9c44a-bfb7-41cf-b03f-44583ead467b', speech_harm_boundary__harm_balancing_reading, forecloses).
narrative_ontology:cs_axiom('7da9c44a-bfb7-41cf-b03f-44583ead467b', foundational, human_dignity_trumps_expression).
narrative_ontology:cs_axiom_status(human_dignity_trumps_expression, holdable).
narrative_ontology:cs_axiom_grounding('7da9c44a-bfb7-41cf-b03f-44583ead467b', human_dignity_trumps_expression, deontological).
narrative_ontology:cs_axiom('7da9c44a-bfb7-41cf-b03f-44583ead467b', foundational, personhood_denial_categorically_unprotected).
narrative_ontology:cs_axiom_status(personhood_denial_categorically_unprotected, holdable).
narrative_ontology:cs_axiom_grounding('7da9c44a-bfb7-41cf-b03f-44583ead467b', personhood_denial_categorically_unprotected, deontological).
narrative_ontology:cs_axiom('7da9c44a-bfb7-41cf-b03f-44583ead467b', secondary, militant_democracy_self_defense).
narrative_ontology:cs_axiom_status(militant_democracy_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('7da9c44a-bfb7-41cf-b03f-44583ead467b', militant_democracy_self_defense, conventional).
narrative_ontology:cs_reference_frame('7da9c44a-bfb7-41cf-b03f-44583ead467b', post_atrocity_dignity_supremacy).
narrative_ontology:cs_drift_state('7da9c44a-bfb7-41cf-b03f-44583ead467b', contemporary_platform_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7da9c44a-bfb7-41cf-b03f-44583ead467b', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__dignity_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, dignity_protected_minority_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, holocaust_memory_communities).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, identity_harm_speakers).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, heterodox_public_commentators).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, minority_faith_critics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, minority_faith_critics).
narrative_ontology:constraint_vindicates(speech_harm_boundary__dignity_reading, human_dignity_supremacy_doctrine).
narrative_ontology:constraint_vindicates(speech_harm_boundary__dignity_reading, militant_democracy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of ethnic, religious, and historically persecuted communities whose equal standing in public discourse the arrangement shields. They receive legal protection from dehumanizing speech, group defamation, and organized incitement. They cannot opt out of the state's definition of their dignity, and some within these communities report that official protection substitutes for their own voice in public argument.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, dignity_protected_minority_groups, beneficiary,
    organized, generational, constrained, national).

% Survivor communities and remembrance institutions whose account of atrocity the arrangement legally shields from denial movements. They gain enforceable public recognition of documented history, but enforcement decisions are made by prosecutors and courts, not by them, and the memory they hold becomes an object of state administration.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, holocaust_memory_communities, beneficiary,
    organized, generational, constrained, continental).

% Individuals whose expression falls into the excluded categories: denial-movement publishers, extremist agitators, and ordinary citizens posting vilifying content. They face fines, imprisonment, and criminal records. Exit means silence, or moving their speech to platforms and jurisdictions outside the enforcing order.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, identity_harm_speakers, payer,
    powerless, biographical, constrained, national).

% Historians, satirists, and polemicists working near the boundary of the excluded categories. Few are ever prosecuted, but the vagueness of the dignity line shrinks the space of lawful provocation; they self-censor, soften arguments, or relocate publication to permissive jurisdictions.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, heterodox_public_commentators, payer,
    moderate, biographical, mobile, continental).

% Members of minority groups who criticize majority religions or dominant national narratives. Nominally inside the protected class, they are empirically over-exposed to prosecution when their speech targets majority sensibilities, experiencing the same machinery that shields them as a threat to their own dissent.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, minority_faith_critics, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__dignity_reading, minority_faith_critics, beneficiary).

% Prosecutors, specialized police units, and courts that define, apply, and incrementally widen the excluded categories. Each widening accrues authority, budget, staffing, and precedent to the enforcement apparatus. They are themselves bound by the constitutional settlement they administer and cannot abandon it without constitutional rupture.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, dignity_law_enforcers, agenda_setter,
    institutional, generational, constrained, national).

% Free-expression advocacy groups locked out of the framework's founding premise. They litigate at the margins, document chilling effects, and publish critiques, but the dignity-supremacy axiom itself is constitutionally entrenched and not open to their contest within the arrangement.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, civil_liberties_organizations, excluded,
    organized, generational, trapped, national).

% Platforms domiciled outside enforcing jurisdictions that host and amplify the excluded categories. Their existence is the principal safety valve keeping the arrangement's suppression short of total closure; they face takedown requests and blocking orders but not prosecution.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, offshore_publishing_platforms, excluded,
    powerful, generational, arbitrage, global).

% Academics tracking divergence across speech regimes, testifying in comparative-law proceedings, and mapping doctrinal migration between jurisdictions. They see the full structure of the arrangement and its rivals without bearing its costs or collecting its protections.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__dignity_reading, dignity_law_enforcers).
narrative_ontology:fixing_cost_class(speech_harm_boundary__dignity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public discourse around equal personhood: establishes a shared baseline that members of all groups participate as rights-bearing equals, removing status contests from marketplace-of-ideas resolution and interrupting the dehumanization cascade that historically precedes organized persecution.
% TRANSFER_FUNCTION: Moves expressive liberty and legal risk exposure from speakers of identity-targeted content to the security of personhood enjoyed by protected groups; moves definitional authority over the limits of public discourse to prosecutorial and judicial institutions.
% ABSENT_VOICES: Absolutist civil-libertarians and prosecuted speakers are present only as defendants or outside critics; the founding premise that dignity trumps expression was settled without their consent. Minority voices who prefer open contest over state protection, and heterodox historians who deny nothing but resent the vagueness, sit outside the conversation that defines the categories.
% DISAPPEARANCE_RATIONALE: If the dignity-subordination arrangement vanished overnight, categorical speech crimes would decriminalize, the enforcement apparatus would dismantle, protected groups would lose their legal shield and re-enter unregulated discursive contest, and offshore platforms would lose their captive audience as denied history re-entered domestic circulation.
% FOUNDING_PROBLEM: The post-atrocity reconstruction problem: mass dehumanizing propaganda demonstrably preceded persecution and genocide, and liberal speech absolutism had proven unable to arrest the spiral. New constitutional orders built human dignity as the supreme value and carved categorical speech exclusions as a defense of the conditions of democratic citizenship.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship documenting the propaganda-to-persecution sequence, UN CERD concluding observations, and European Court of Human Rights case law upholding certain dignitary restrictions corroborate the founding problem from outside the benefiting parties. The same court's Perinçek line simultaneously attests that the categorical scope remains contested, so corroboration covers the problem, not the arrangement's full extent.
narrative_ontology:disappearance_verdict(speech_harm_boundary__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__dignity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__dignity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_harm_boundary__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__dignity_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the arrangement's operating mode is categorical removal of speech from protection: no balancing escape hatch exists for the excluded classes, liability is criminal, and the category boundary has widened monotonically across the interval. Suppression (0.66) is a raw structural property, unscaled by power or scope: it reflects dedicated enforcement machinery — specialized units, platform takedown cooperation, blocking orders — that the arrangement requires to hold. Theater is honestly low-to-moderate (0.26): most enforcement is functional prosecution of real conduct, with a growing performative share as the founding perpetrator generation dies out and enforcement shifts to symbolic memory-defense and marginal online figures. Accessibility collapse is moderate (0.48): within the framework the excluded categories have no legal alternative route, but offshore platforms and private discourse keep exits partly open. Resistance is substantial (0.58): sustained scholarly critique, civil-liberties litigation, political backlash, and Strasbourg pushback meet the arrangement continuously. Claimed type is tangled_rope on structural grounds independent of these metrics: the arrangement possesses a genuine coordination function (equal-personhood baseline), asymmetric extraction through the same structure (speakers pay, enforcers and protected classes gain), and requires active enforcement to hold. The measurement series run on one shared time grid — every tracked metric is authored at every examined time point — showing monotonic accumulation of extraction and enforcement capacity rather than oscillation.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the identity_harm_speaker seat the arrangement operates as enforced extraction with no exit: categorical liability, criminal record, silence or flight. From the protected-group seat the same structure operates as a load-bearing coordination device their civic equality depends on. From the enforcer seat it operates as legitimate administration of a constitutional settlement — with the complication that each widening of the categories accrues authority and budget to that seat, so the administrator is also a partial collector. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Protected groups and memory communities sit near the beneficiary end (low d): the arrangement subsidizes their civic standing and they bear little of its cost. Identity-harm speakers sit near the full-target end (high d): they supply the entire transfer, are identity-positioned (their speech is the regulated act), and their exits run through silence or offshore arbitrage. Heterodox commentators and minority faith critics bear diffuse but real costs — chilling and asymmetric exposure respectively — placing them high-d despite never appearing in a courtroom. The enforcer seat is the one place the structural derivation would mislead: role-derived d would read prosecutors as neutral administrators, but the arrangement's growth path channels authority, staffing, and precedent to that seat, making it a partial beneficiary. A directionality override sets the institutional atom to d=0.30 to record that capture-shaped self-interest; only one agent occupies that atom in this story, so the override is well-scoped.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — arresting the dehumanization-to-persecution cascade — remains live, corroborated by sources outside the benefiting parties, so this is not a resolved-mandatrophy case and the arrangement is not drifting piton-ward yet. The tangled_rope classification is what prevents both mislabels: reading it as pure rope would erase the speaker extraction and the enforcer's accumulating stake; reading it as pure snare would erase the genuine coordination function that protected groups cannot currently replace. The theater_ratio series is the early-warning instrument: if the founding problem ever dies (dehumanization risk extinguished) while enforcement persists as memory ritual and marginal-figure prosecution, the rising theater trajectory documented here is the signature the transition would ride on.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the speech_harm_boundary kernel (dignity_reading); which reading governs a given jurisdiction''s arrangement, and how would the classification change under the absolutist_reading or harm_balancing_reading siblings?',
    'Jurisdictional adoption analysis mapping each legal order to its operative reading, then generating the sibling stories and comparing computed classifications across the family.',
    'Under the absolutist_reading the victim set collapses toward none and extraction approaches zero; under the harm_balancing_reading extraction becomes case-contingent and moderate. The tangled_rope verdict here holds only for this reading''s arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: reading-indexed classification within a three-reading kernel contest.').

omega_variable(
    asymmetric_enforcement_pattern,
    'Are the excluded categories enforced symmetrically across groups, or selectively against minority critics of majority sensibilities and political outsiders?',
    'Prosecution-disparity study: compare charging rates, conviction rates, and sentence severity across speaker group and target group for conduct falling in the same statutory categories.',
    'Demonstrated asymmetry would concentrate effective extraction on political and minority targets, pushing the arrangement''s computed profile toward snare-flavored operation despite its genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_enforcement_pattern, empirical, 'Whether enforcement of the dignity categories tracks dignitary harm or target identity.').

omega_variable(
    definitional_authority_capture,
    'Does the boundary of ''personhood-denying speech'' track the harm the categories were built to prevent, or the enforcement apparatus''s institutional interest in widening them?',
    'Trace category-widening episodes against measured harm incidence: if expansions consistently precede rather than follow documented harm increases, definitional authority has detached from the founding problem.',
    'Detachment would confirm the enforcer-seat override (d=0.30) as understated and support reclassifying the enforcement layer as extraction-driven rather than protection-driven.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definitional_authority_capture, empirical, 'Whether the category boundary serves victims or the apparatus that administers it.').

omega_variable(
    chilling_effect_extent,
    'How far does the chilling effect extend beyond prosecuted cases into lawful historiography, satire, and criticism of religion?',
    'Survey and publication-pattern analysis comparing self-reported avoidance and topic dropout among commentators inside versus outside enforcing jurisdictions.',
    'A wide shadow would raise effective suppression above the structural measure and enlarge the heterodox-commentator victim class; a narrow shadow would confine the burden to actual speakers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_extent, empirical, 'Size of the unprosecuted behavioral shadow cast by the categorical exclusions.').

omega_variable(
    paternalism_versus_voice,
    'Do protected-group members experience the arrangement as protection of their civic standing, or as state substitution for their own voice in public argument?',
    'Attitudinal research within protected communities, weighted by whether members litigate for or against expansion of the categories.',
    'If protection reads as paternalism to its supposed beneficiaries, the coordination-function leg of the tangled_rope verdict weakens and the arrangement slides toward extraction with a borrowed constituency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalism_versus_voice, preference, 'Whether the beneficiary class endorses the protection or experiences it as displacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__dignity_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shb_dignity_tr_t0, speech_harm_boundary__dignity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(shb_dignity_tr_t12, speech_harm_boundary__dignity_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement(shb_dignity_tr_t25, speech_harm_boundary__dignity_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(shb_dignity_tr_t38, speech_harm_boundary__dignity_reading, theater_ratio, 38, 0.18).
narrative_ontology:measurement(shb_dignity_tr_t50, speech_harm_boundary__dignity_reading, theater_ratio, 50, 0.21).
narrative_ontology:measurement(shb_dignity_tr_t62, speech_harm_boundary__dignity_reading, theater_ratio, 62, 0.24).
narrative_ontology:measurement(shb_dignity_tr_t75, speech_harm_boundary__dignity_reading, theater_ratio, 75, 0.26).

% Extraction over time
narrative_ontology:measurement(shb_dignity_be_t0, speech_harm_boundary__dignity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(shb_dignity_be_t12, speech_harm_boundary__dignity_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(shb_dignity_be_t25, speech_harm_boundary__dignity_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(shb_dignity_be_t38, speech_harm_boundary__dignity_reading, base_extractiveness, 38, 0.58).
narrative_ontology:measurement(shb_dignity_be_t50, speech_harm_boundary__dignity_reading, base_extractiveness, 50, 0.63).
narrative_ontology:measurement(shb_dignity_be_t62, speech_harm_boundary__dignity_reading, base_extractiveness, 62, 0.69).
narrative_ontology:measurement(shb_dignity_be_t75, speech_harm_boundary__dignity_reading, base_extractiveness, 75, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(shb_dignity_su_t0, speech_harm_boundary__dignity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(shb_dignity_su_t12, speech_harm_boundary__dignity_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(shb_dignity_su_t25, speech_harm_boundary__dignity_reading, suppression_requirement, 25, 0.49).
narrative_ontology:measurement(shb_dignity_su_t38, speech_harm_boundary__dignity_reading, suppression_requirement, 38, 0.54).
narrative_ontology:measurement(shb_dignity_su_t50, speech_harm_boundary__dignity_reading, suppression_requirement, 50, 0.59).
narrative_ontology:measurement(shb_dignity_su_t62, speech_harm_boundary__dignity_reading, suppression_requirement, 62, 0.64).
narrative_ontology:measurement(shb_dignity_su_t75, speech_harm_boundary__dignity_reading, suppression_requirement, 75, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__dignity_reading, identity_coordination).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__harm_balancing_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the speech-harm boundary' conflates three structurally distinct arrangements. The dignity_reading (this file) holds categorical, non-balanceable exclusions with heavy speaker burden and high epsilon; the absolutist_reading holds near-absolute protection with negligible extraction; the harm_balancing_reading holds case-contingent proportionality with moderate, demonstrated-harm-gated extraction. Each carries its own epsilon, victim set, and classification. The absolutist reading functions as the upstream reference (its tradition supplies the standard critique vocabulary), and the dignity reading exerts downstream pressure on the harm_balancing reading by raising the floor of restriction that balancing regimes must justify against. All three files link one another through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_harm_boundary__dignity_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
