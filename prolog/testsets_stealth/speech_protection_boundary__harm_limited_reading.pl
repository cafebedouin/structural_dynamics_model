% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__harm_limited_reading, []).

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
 *   constraint_id: speech_protection_boundary__harm_limited_reading
 *   human_readable: Speech Protection Boundary — Harm-Limited Reading (Dignity-Conditioned Protection)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the speech protection boundary
 *   kernel: the harm-limited reading, under which speech protection is
 *   conditional on absence of significant harm to dignity, equality, and
 *   freedom from harassment. The arrangement narrows the protected set — hate
 *   speech, harassment, and coded dog whistles fall outside it — and installs
 *   the state as gatekeeper of the boundary, with the attendant abuse risk
 *   the reading itself acknowledges. It is a constructed post-war settlement,
 *   not a natural feature: it requires standing enforcement machinery (human
 *   rights commissions, tribunals, platform conscription statutes) to hold,
 *   it has identifiable beneficiaries (protected claimants, and the
 *   enforcement apparatus that administers the protection), and it imposes
 *   real, asymmetrically distributed costs on speakers and platforms.
 *   CONSTRAINT FAMILY: the colloquial label 'speech protection' decomposes
 *   into three structurally distinct constraints per the epsilon-invariance
 *   principle. This file's epsilon (0.5) is authored for THIS arrangement —
 *   the state-gatekeeper boundary with its narrowed protected set — as this
 *   reading's own lights assess it: genuine protection function, real speaker
 *   costs, acknowledged gatekeeper risk. The absolutist sibling
 *   (near-absolute protection, exception confined to imminent lawless action)
 *   would carry near-zero state-side extraction on its own referent but a
 *   different victim structure — harassed targets left unprotected. The
 *   balancing sibling (case-by-case weighing) would carry case-by-case
 *   variability rather than a categorical boundary. The readings are linked
 *   as one kernel family via network.affects_constraints; this file does not
 *   average across them.
 *
 * KEY AGENTS:
 *   - anti_discrimination_enforcement_bodies: agenda-setter and institutional beneficiary (institutional/identity-locked) — administers the boundary, defines 'significant harm' in practice, accrues jurisdiction and budget with each recognized category
 *   - harassment_targets: primary beneficiary (organized/constrained) — converts individual vulnerability into actionable claims through the enforcement machinery
 *   - marginalized_identity_groups: structural beneficiary (organized/generational) — the boundary shapes the discourse environment their members inhabit
 *   - sanctioned_speakers: primary target (moderate/trapped) — bears fines, published findings, and their follow-on consequences with no clean exit
 *   - political_dissenters: secondary target with partial benefit (moderate/constrained) — bears the elastic-category risk while also benefiting from a less hostile discourse environment
 *   - social_media_platforms: conscripted enforcer-payer (institutional/constrained) — operationalizes the harm standard at scale under asymmetric penalty structures
 *   - civil_liberties_advocates: analytical observer (institutional/analytical) — maps the boundary's drift adversarially from outside the enforcement apparatus
 *   - general_public: diffuse beneficiary-payer (moderate/generational) — experiences the guarded discourse environment and the narrowed protected set simultaneously
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, 0.5).
domain_priors:suppression_score(speech_protection_boundary__harm_limited_reading, 0.6).
domain_priors:theater_ratio(speech_protection_boundary__harm_limited_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__harm_limited_reading, "Speech Protection Boundary — Harm-Limited Reading (Dignity-Conditioned Protection)").
narrative_ontology:topic_domain(speech_protection_boundary__harm_limited_reading, "constitutional/political").

domain_priors:requires_active_enforcement(speech_protection_boundary__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__harm_limited_reading, '9dfe48ba-af7d-444c-8548-399fefa7b1f4').
narrative_ontology:cs_kernel_codification('9dfe48ba-af7d-444c-8548-399fefa7b1f4', fixed_text).
narrative_ontology:cs_authority_grounding('9dfe48ba-af7d-444c-8548-399fefa7b1f4', lineage).
narrative_ontology:cs_interpretation_layer_present('9dfe48ba-af7d-444c-8548-399fefa7b1f4').
narrative_ontology:cs_reading_relation('9dfe48ba-af7d-444c-8548-399fefa7b1f4', speech_protection_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('9dfe48ba-af7d-444c-8548-399fefa7b1f4', speech_protection_boundary__balancing_reading, influences).
narrative_ontology:cs_axiom('9dfe48ba-af7d-444c-8548-399fefa7b1f4', foundational, dignity_harm_justifies_restriction).
narrative_ontology:cs_axiom_status(dignity_harm_justifies_restriction, holdable).
narrative_ontology:cs_axiom_grounding('9dfe48ba-af7d-444c-8548-399fefa7b1f4', dignity_harm_justifies_restriction, deontological).
narrative_ontology:cs_axiom('9dfe48ba-af7d-444c-8548-399fefa7b1f4', foundational, state_gatekeeping_for_equal_participation).
narrative_ontology:cs_axiom_status(state_gatekeeping_for_equal_participation, holdable).
narrative_ontology:cs_axiom_grounding('9dfe48ba-af7d-444c-8548-399fefa7b1f4', state_gatekeeping_for_equal_participation, instrumental).
narrative_ontology:cs_reference_frame('9dfe48ba-af7d-444c-8548-399fefa7b1f4', post_war_human_rights_settlement).
narrative_ontology:cs_drift_state('9dfe48ba-af7d-444c-8548-399fefa7b1f4', contemporary_platform_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9dfe48ba-af7d-444c-8548-399fefa7b1f4', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__harm_limited_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, harassment_targets).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, marginalized_identity_groups).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, anti_discrimination_enforcement_bodies).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, general_public).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, sanctioned_speakers).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, political_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, political_dissenters).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, social_media_platforms).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Human rights commissions, equality bodies, and prosecuting authorities initiate and litigate harm claims under dignity and harassment standards, set enforcement priorities, and propose expansions of the actionable-harm categories to legislators. Their budgets, caseloads, and jurisdiction grow with each recognized category, and decades of mandate-building have fused the institutions with the protection mission: proposals to narrow their remit are processed internally as attacks on the mission itself rather than as ordinary policy options.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, anti_discrimination_enforcement_bodies, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__harm_limited_reading, anti_discrimination_enforcement_bodies, beneficiary).

% People subjected to sustained denigration, coordinated harassment campaigns, or group-directed intimidation. Private confrontation and ordinary tort remedies have historically failed them, so their access to redress runs through the human rights machinery; they have a direct stake in the breadth and continuity of the actionable-harm categories and in the enforcement bodies that hear their claims.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, harassment_targets, beneficiary,
    organized, biographical, constrained, national).

% Groups whose members face recurring dignity attacks in public discourse and whose collective vulnerability cannot be answered member-by-member. The boundary converts that vulnerability into actionable claims and shapes the discourse environment their members inhabit; their advocacy organizations litigate test cases and press for broader categories of actionable harm.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, marginalized_identity_groups, beneficiary,
    organized, generational, constrained, national).

% People whose expression has been found to cross the harm line — fines, published findings, takedown and removal orders, employment consequences. The finding itself is a public record that follows them, appeal is costly and slow, and the injury they seek escape from is the finding, so once adjudicated they have no clean exit from the sanction's effects.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, sanctioned_speakers, payer,
    moderate, biographical, trapped, national).

% Speakers whose political expression — criticism of migration policy, contested questions of sex and gender, religious commentary — risks classification as coded harm. They bear the chilling cost of elastic categories and the risk that their speech is recharacterized, while also benefiting from a discourse environment in which they themselves are less likely to face coordinated denigration. Their exit is voice: litigation, appeal, and political contestation of overbroad applications.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, political_dissenters, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__harm_limited_reading, political_dissenters, beneficiary).

% Platforms are conscripted as first-line adjudicators under notice-and-takedown and online-safety statutes: they must classify harm at scale, remove flagged content on penalty of heavy fines, and build moderation taxonomies that operationalize someone else's harm standard. Asymmetric penalties make over-removal their rational response, and they cannot exit the national markets where the rules apply without abandoning their user bases.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, social_media_platforms, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__harm_limited_reading, social_media_platforms, agenda_setter).

% Organizations that litigate against overbroad harm findings, publish overbreadth audits, and defend sanctioned speakers. They accept the reality of group-directed harassment while contesting the remedy's breadth and the elasticity of its categories; their seat is adversarial-analytical — they map the boundary's movement without administering it and without bearing its sanctions.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, civil_liberties_advocates, observer,
    institutional, generational, analytical, national).

% Everyone else in the discourse environment: they experience a public sphere with less open denigration and intimidation, and they carry the diffuse costs of a narrower protected set — margin-of-error self-censorship and the precedent value of each category expansion, which binds future speakers who had no say in it. They are represented in the system but not seated in it.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, general_public, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__harm_limited_reading, general_public, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__harm_limited_reading, anti_discrimination_enforcement_bodies).
narrative_ontology:fixing_cost_class(speech_protection_boundary__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: members of groups facing dignity attacks and harassment cannot defend themselves individually against diffuse, coordinated denigration, and private remedies have historically failed. The boundary centralizes protection — converting diffuse dignity harms into actionable claims adjudicated by specialized bodies — and coordinates the discourse environment around a shared, enforceable floor of mutual civility.
% TRANSFER_FUNCTION: Moves expressive liberty and adjudicatory discretion. From speakers: protected-set breadth, sanction costs, and the chilling margin. From platforms: compliance costs, moderation labor, and over-removal liability. To protected claimants: remedies and a guarded discourse environment. To enforcement bodies: jurisdiction, budget, and category-expansion initiative.
% ABSENT_VOICES: Speakers whose expression is classified as harmful do not sit on the bodies that define harm — their perspective enters only as a defense at hearing, never in standard-setting. Absolutist-leaning jurists and civil libertarians are heard but routinely outvoted in the forums that draw the categories. Future speakers, who will be bound by each expansion's precedent, are structurally absent from every proceeding that expands the unprotected set. The sanctioned speaker's recurring claim — that harm determinations track political valence — has no institutional seat.
% DISAPPEARANCE_RATIONALE: If the harm-limited boundary vanished overnight, harassment and dignity claims would lose their adjudicative home, enforcement bodies would lose their core jurisdiction, platform moderation mandates would collapse to voluntary policy, and the discourse environment would reorganize around the nearest surviving baseline — litigation-tested tort and criminal harassment law — with the protected set expanding to near-absolutist scope in jurisdictions without an alternative settlement.
% FOUNDING_PROBLEM: The post-war settlement responded to a demonstrated failure: neutral speech rules left members of vulnerable groups unprotected against sustained, coordinated denigration and harassment, and no private remedy could answer a hostile discourse environment. Human rights instruments and codes were built to make dignity and equality enforceable against expressive conduct.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated from outside the beneficiary set: civil liberties organizations — the arrangement's most consistent institutional critics — acknowledge that group-directed harassment is real and under-remedied at baseline, while contesting the remedy's breadth; criminological and social-psychological research documents the harms of sustained group-directed harassment; and even jurisdictions with near-absolutist doctrine (the Brandenburg line) carve out harassment-adjacent categories, an implicit acknowledgment that the founding problem exists. No serious party attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(speech_protection_boundary__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__harm_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__harm_limited_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_boundary__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__harm_limited_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.5 because the arrangement genuinely extracts from identifiable seats — speakers bear sanctions and a chilling margin, platforms bear compliance and over-removal costs, and the state accumulates discretionary gatekeeping power — while the protection it delivers is real and reaches its intended recipients. Suppression is 0.6: persistence requires active machinery (tribunals, notice-and-takedown regimes, platform fines), not voluntary compliance, but the suppressive force is bounded by the harm threshold rather than general. Theater is 0.25: adjudication is mostly functional — real claims, real evidence, real remedies — with a growing performative share (symbolic prosecutions, dignity-signaling enforcement) as the apparatus matures. Accessibility_collapse is 0.5: once speakers understand the boundary, alternatives persist — reframing, protected channels, litigation — so the constraint does not collapse the option space the way a natural law would. Resistance is 0.6: civil liberties litigation, jurisdiction-level repeals, and sustained political contestation are structural features of the arrangement's operation, not noise. The measurement series run on ONE shared time grid (t = 0, 8, 16, 25, 33, 42, 50, spanning the settlement's maturation from the human-rights-code era through the platform-conscription era) so every tracked metric is authored at every examined point. All three trajectories rise: extraction accumulation as categories expand, enforcement intensification as the machinery hardens (tribunal growth, then the platform layer), and slow theater growth as performative enforcement scales with the apparatus. The trajectory is monotonic in aggregate; jurisdiction-level contractions (e.g., repeal of specific hate-speech provisions) occur but are absorbed within a decade by transnational enforcement growth — the ratchet, not the cycle, is the operative pattern, and no cyclical dynamics are claimed.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the finding. From the enforcement seat, the arrangement is its mission: success is measured in categories recognized and claims upheld, and the boundary's growth reads as the settlement working. From the sanctioned-speaker and dissenter seats, the same structure operates as an elastic line that moves toward their expression, administered by bodies whose budgets grow with each expansion — protection and threat in one apparatus. The platform seat experiences a third structure: asymmetric liability that makes over-removal rational regardless of the standard's content. The observer seat sees the drift the operator seat does not acknowledge. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for harassment_targets and marginalized_identity_groups — the arrangement subsidizes them (remedies, a guarded discourse environment) at little structural cost, and their constrained exit (their redress runs through the machinery) locks them in as beneficiaries rather than targets. anti_discrimination_enforcement_bodies derive low d from their beneficiary role, but note the receipt distinction: they benefit AND capture — jurisdiction, budget, and category-expansion initiative accrue to them specifically, which is why they hold gain_flow. Victims derive high d: sanctioned_speakers are trapped (the finding is the injury; no clean exit) and sit nearest the full-target end; political_dissenters sit somewhat lower because their secondary beneficiary position (they too benefit from reduced denigration) partially offsets their exposure — the derivation from dual role plus constrained exit captures this without an override. social_media_platforms carry high d despite institutional power: asymmetric penalties and market-lock-in make them payers with agenda-setting obligations but no agenda-setting discretion over the standard itself. general_public sits near symmetric — diffuse benefit, diffuse cost. No directionality overrides are authored: the role-plus-exit derivation produces the correct structure for every seat, and an override keyed on a power atom would flatten genuinely different moderate-power seats (trapped sanctioned speakers versus constrained dissenters) into one value.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. First, the mountain mislabel: the settlement presents itself as the natural order of rights — dignity protection as simply what rights ARE — but it is a constructed post-war arrangement with identifiable beneficiaries, standing enforcement machinery, and a contested sibling set; if it were claimed as a mountain, its declared beneficiaries would trigger false-summit evaluation, and the omegas here document the natural-law-versus-constructed ambiguity directly. Second, the snare mislabel: a purely extractive reading — state censorship dressed as protection — misses that the founding problem is live and independently corroborated, that the protection reaches its intended recipients, and that the arrangement solves a collective-action problem private remedies demonstrably could not. The tangled_rope claim holds both truths: genuine coordination AND asymmetric extraction through the same structure, held together by active enforcement. On obsolescence: the founding problem is live (harassment and dignity attacks persist, corroborated from outside the beneficiary set) and the world rearranges without the arrangement, so no capture/zombie mismatch fires — but the elastic-threshold and enforcement-capture omegas mark exactly the conditions under which the arrangement would drift toward a dead-mandate ratchet: if the founding problem were ever genuinely solved while the enforcement apparatus persisted, the status-by-verdict mismatch would flag it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Which reading does the speech protection boundary kernel actually bear — is the dignity-harm condition part of the kernel itself, or is this file one interpretive layer over a kernel whose text underdetermines the boundary?',
    'Structural analysis of the kernel''s source texts (constitutional provisions, human rights instruments) for whether they entrench a harm condition or leave the boundary open; comparative tracing of which reading each jurisdiction''s doctrine actually instantiates.',
    'If the kernel itself is harm-neutral and merely protective, this reading is one contestable interpretation rather than the kernel''s content, and the sibling readings are equally faithful instantiations with materially different victim sets; if the harm condition is kernel-deep, the absolutist reading is not a rival constraint but a rejection of the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this constraint is the harm_limited_reading of kernel speech_protection_boundary; siblings are absolutist_reading and balancing_reading.').

omega_variable(
    gatekeeper_abuse_trajectory,
    'Does the state''s harm-determination discretion get weaponized against political dissent and unpopular-but-protected viewpoints, and at what rate relative to its use against genuine harassment?',
    'Longitudinal analysis of sanction targets: distribution of political valence among adjudicated speakers, overbreadth reversal rates on appeal, and comparative outcomes for identical expressive conduct across jurisdictions with different readings of the same kernel.',
    'Rising weaponization would shift the arrangement''s operative function from protection toward dissent management — the payer seats would compute as full targets and the structure would drift snare-ward; contained abuse supports the hybrid coordination/extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeper_abuse_trajectory, empirical, 'The expected structural delta names state gatekeeping with attendant abuse risk; this omega measures whether the risk is materializing.').

omega_variable(
    significant_harm_threshold_elasticity,
    'Is ''significant harm to dignity, equality, and freedom from harassment'' a stable, administrable threshold, or is it inherently elastic — expanding monotonically with enforcement priorities and advocacy pressure?',
    'Comparative doctrinal analysis across jurisdictions and decades: does the unprotected set (hate speech, harassment, coded dog whistles, microaggression-adjacent conduct) expand monotonically, and do contractions (e.g., repeal of specific provisions) persist or get reabsorbed?',
    'An elastic threshold makes each expansion a ratchet — extraction accumulates irreversibly and the payer seats'' effective burden grows faster than the formal rules suggest; a stable threshold keeps the arrangement a contained coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(significant_harm_threshold_elasticity, empirical, 'Whether the boundary''s operative line is fixed or drifts under enforcement incentives.').

omega_variable(
    enforcement_capture_vs_protection_demand,
    'Is the expansion of the enforcement apparatus driven by beneficiary demand for protection, or by institutional self-interest in jurisdiction growth?',
    'Trace the origin of category expansions: legislative mandate versus agency initiative; correlate enforcement-body budget and caseload growth with complaint volume versus with jurisdictional reach; examine internal positioning documents around proposed narrowing.',
    'Self-driven expansion confirms the enforcement seat as the structural capturer of the arrangement''s gains and strengthens the extraction side of the hybrid; beneficiary-driven expansion keeps the coordination function dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capture_vs_protection_demand, empirical, 'Who actually drives the boundary''s growth — the protected or the protectors.').

omega_variable(
    chilling_effect_magnitude,
    'How much protected speech is deterred by the boundary''s existence — beyond formal sanctions — among speakers who never appear before any tribunal?',
    'Survey and experimental evidence comparing stated willingness to engage in contested political expression inside harm-limited jurisdictions versus absolutist-baseline jurisdictions, controlling for topic and speaker position.',
    'High chilling means the arrangement''s effective burden on the payer seats substantially exceeds its formal footprint — the speaker-side seats would compute nearer the full-target end than their sanction rates suggest, and the extraction side of the hybrid is understated by formal data.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilling_effect_magnitude, empirical, 'The gap between formal sanction incidence and the boundary''s real behavioral shadow.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__harm_limited_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_harm_limited_tr_t0, speech_protection_boundary__harm_limited_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(speech_harm_limited_tr_t8, speech_protection_boundary__harm_limited_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(speech_harm_limited_tr_t16, speech_protection_boundary__harm_limited_reading, theater_ratio, 16, 0.17).
narrative_ontology:measurement(speech_harm_limited_tr_t25, speech_protection_boundary__harm_limited_reading, theater_ratio, 25, 0.19).
narrative_ontology:measurement(speech_harm_limited_tr_t33, speech_protection_boundary__harm_limited_reading, theater_ratio, 33, 0.21).
narrative_ontology:measurement(speech_harm_limited_tr_t42, speech_protection_boundary__harm_limited_reading, theater_ratio, 42, 0.23).
narrative_ontology:measurement(speech_harm_limited_tr_t50, speech_protection_boundary__harm_limited_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(speech_harm_limited_be_t0, speech_protection_boundary__harm_limited_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(speech_harm_limited_be_t8, speech_protection_boundary__harm_limited_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(speech_harm_limited_be_t16, speech_protection_boundary__harm_limited_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(speech_harm_limited_be_t25, speech_protection_boundary__harm_limited_reading, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(speech_harm_limited_be_t33, speech_protection_boundary__harm_limited_reading, base_extractiveness, 33, 0.47).
narrative_ontology:measurement(speech_harm_limited_be_t42, speech_protection_boundary__harm_limited_reading, base_extractiveness, 42, 0.49).
narrative_ontology:measurement(speech_harm_limited_be_t50, speech_protection_boundary__harm_limited_reading, base_extractiveness, 50, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(speech_harm_limited_su_t0, speech_protection_boundary__harm_limited_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(speech_harm_limited_su_t8, speech_protection_boundary__harm_limited_reading, suppression_requirement, 8, 0.43).
narrative_ontology:measurement(speech_harm_limited_su_t16, speech_protection_boundary__harm_limited_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(speech_harm_limited_su_t25, speech_protection_boundary__harm_limited_reading, suppression_requirement, 25, 0.51).
narrative_ontology:measurement(speech_harm_limited_su_t33, speech_protection_boundary__harm_limited_reading, suppression_requirement, 33, 0.54).
narrative_ontology:measurement(speech_harm_limited_su_t42, speech_protection_boundary__harm_limited_reading, suppression_requirement, 42, 0.57).
narrative_ontology:measurement(speech_harm_limited_su_t50, speech_protection_boundary__harm_limited_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__harm_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__balancing_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'speech protection' covers three structurally distinct claims and is decomposed into a three-story kernel family. This file instantiates the harm-limited reading only: its epsilon (0.5) is authored for the state-gatekeeper arrangement with the narrowed protected set, assessed by this reading's own lights — genuine protection function, real speaker and platform costs, acknowledged gatekeeper abuse risk. The absolutist sibling would carry near-zero state-side extraction on its own referent but a structurally different victim set (harassment targets left without redress); the balancing sibling would carry adjudication-by-adjudication variability instead of a categorical line. Upstream/downstream structure within the family: this reading's reference frame (the post-war human rights settlement) supplies the dignity and equality values that the balancing reading's weighing exercises must take as inputs — the harm-limited reading structurally influences the balancing reading without foreclosing it, while directly contradicting the absolutist reading's core premise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
