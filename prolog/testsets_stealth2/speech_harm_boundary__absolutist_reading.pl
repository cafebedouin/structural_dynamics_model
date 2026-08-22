% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__absolutist_reading, []).

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
 *   constraint_id: speech_harm_boundary__absolutist_reading
 *   human_readable: Near-Absolute Speech Protection with Narrow Categorical Exceptions (Absolutist Reading)
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   The standing arrangement under contest is a constitutional order in which
 *   expressive protection operates near-absolutely: a harm override threshold
 *   set extremely high, four narrow categorical exclusions (incitement, true
 *   threats, defamation, obscenity), and everything else protected —
 *   including expression that inflicts serious psychological, reputational,
 *   and participatory harm on identifiable targets, who bear those costs
 *   without remedy. This file instantiates ONE reading of the
 *   speech_harm_boundary kernel; the harm-balancing and dignity readings are
 *   separate constraints (separate files, linked via
 *   network.affects_constraints), not positions described inside this one.
 *   KEY AGENTS (by structural relationship): apex_constitutional_court —
 *   agenda-setter (institutional/identity_locked), administers the boundary
 *   and strikes down alternative regimes; mass_media_corporations —
 *   concentrated beneficiary (powerful/arbitrage); platform_operators —
 *   concentrated beneficiary and principal receiver of the arrangement's
 *   gains (institutional/arbitrage); political_provocateurs — beneficiary
 *   (moderate/mobile); dissenting_citizens — diffuse coordination beneficiary
 *   (moderate/mobile); coordinated_harassment_targets — primary target
 *   (powerless/trapped); degraded_minority_communities — target with internal
 *   speaker heterogeneity (organized/constrained);
 *   target_community_representatives — excluded voice (moderate/constrained);
 *   comparative_constitutional_scholars — analytical observer
 *   (analytical/analytical). Claim/metric independence is preserved:
 *   claimed_type is authored as tangled_rope because the arrangement
 *   genuinely coordinates (binding the censor) while genuinely extracting
 *   (concentrated gains, uncompensated concentrated costs) — while the
 *   reading's own rhetoric presents the arrangement as a necessary and
 *   largely benign settlement. That divergence between self-presentation and
 *   structure is exactly the datum the corpus exists to take.
 *
 * KEY AGENTS:
 *   - apex_constitutional_court: agenda-setter (institutional / identity_locked) — administers the boundary, defines the four unprotected categories, strikes down legislative attempts to regulate harmful-but-protected expression
 *   - mass_media_corporations: concentrated beneficiary (powerful / arbitrage) — publishes under near-total protection with liability confined to a narrow channel
 *   - platform_operators: concentrated beneficiary and principal receiver of gains (institutional / arbitrage) — monetizes amplified expression shielded from liability
 *   - political_provocateurs: beneficiary (moderate / mobile) — builds audience and movement through consequence-free transgression
 *   - dissenting_citizens: diffuse coordination beneficiary (moderate / mobile) — ordinary speakers whose dissent rides the presumption of protection
 *   - coordinated_harassment_targets: primary target (powerless / trapped) — bears organized-harassment and degradation costs with no remedy channel
 *   - degraded_minority_communities: target with internal speaker heterogeneity (organized / constrained) — simultaneously protected speakers and bearing communities of demeaning mass expression
 *   - target_community_representatives: excluded voice (moderate / constrained) — presses for standing and remedy channels from outside the adjudicating coalition
 *   - comparative_constitutional_scholars: analytical observer (analytical / analytical) — maps how sibling jurisdictions draw the same boundary differently
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, 0.38).
domain_priors:suppression_score(speech_harm_boundary__absolutist_reading, 0.62).
domain_priors:theater_ratio(speech_harm_boundary__absolutist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__absolutist_reading, "Near-Absolute Speech Protection with Narrow Categorical Exceptions (Absolutist Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__absolutist_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__absolutist_reading, '0540c40e-e891-4c87-9711-0a5b6895eb28').
narrative_ontology:cs_kernel_codification('0540c40e-e891-4c87-9711-0a5b6895eb28', fixed_text).
narrative_ontology:cs_authority_grounding('0540c40e-e891-4c87-9711-0a5b6895eb28', lineage).
narrative_ontology:cs_interpretation_layer_present('0540c40e-e891-4c87-9711-0a5b6895eb28').
narrative_ontology:cs_reading_relation('0540c40e-e891-4c87-9711-0a5b6895eb28', speech_harm_boundary__harm_balancing_reading, forecloses).
narrative_ontology:cs_reading_relation('0540c40e-e891-4c87-9711-0a5b6895eb28', speech_harm_boundary__dignity_reading, forecloses).
narrative_ontology:cs_axiom('0540c40e-e891-4c87-9711-0a5b6895eb28', foundational, harm_weighing_cannot_bound_restriction).
narrative_ontology:cs_axiom_status(harm_weighing_cannot_bound_restriction, holdable).
narrative_ontology:cs_axiom_grounding('0540c40e-e891-4c87-9711-0a5b6895eb28', harm_weighing_cannot_bound_restriction, empirically_contingent).
narrative_ontology:cs_axiom('0540c40e-e891-4c87-9711-0a5b6895eb28', foundational, offense_does_not_forfeit_protection).
narrative_ontology:cs_axiom_status(offense_does_not_forfeit_protection, holdable).
narrative_ontology:cs_axiom_grounding('0540c40e-e891-4c87-9711-0a5b6895eb28', offense_does_not_forfeit_protection, deontological).
narrative_ontology:cs_axiom('0540c40e-e891-4c87-9711-0a5b6895eb28', secondary, target_costs_uncompensated_by_design).
narrative_ontology:cs_axiom_status(target_costs_uncompensated_by_design, holdable).
narrative_ontology:cs_axiom_grounding('0540c40e-e891-4c87-9711-0a5b6895eb28', target_costs_uncompensated_by_design, instrumental).
narrative_ontology:cs_reference_frame('0540c40e-e891-4c87-9711-0a5b6895eb28', bright_line_categorical_settlement).
narrative_ontology:cs_drift_state('0540c40e-e891-4c87-9711-0a5b6895eb28', digital_amplification_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0540c40e-e891-4c87-9711-0a5b6895eb28', '2026-08-05T14:32:00Z').
narrative_ontology:cs_kernel_id(speech_harm_boundary__absolutist_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, dissenting_citizens).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, mass_media_corporations).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, political_provocateurs).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, platform_operators).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, coordinated_harassment_targets).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, degraded_minority_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, apex_constitutional_court).
narrative_ontology:constraint_vindicates(speech_harm_boundary__absolutist_reading, censorship_ratchet_thesis).
narrative_ontology:constraint_vindicates(speech_harm_boundary__absolutist_reading, self_governing_deliberation_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates every challenge to expressive regulation and every claimed speech interest. Holds the boundary at four narrow unprotected categories and reviews, usually striking down, legislative attempts to regulate harmful-but-unprotected expression. Its prestige, caseload architecture, and accumulated interpretive capital are bound to the settlement it maintains; departing from the line would mean repudiating decades of its own precedent and self-description.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, apex_constitutional_court, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__absolutist_reading, apex_constitutional_court, beneficiary).

% Publish, editorialize, and entertain under near-total protection, with liability confined to a narrow falsity-plus-status channel. Controversial coverage converts directly to audience share. When any jurisdiction tightens expression rules, they can litigate, relocate domiciles, or shift distribution channels across borders.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, mass_media_corporations, beneficiary,
    powerful, biographical, arbitrage, global).

% Host and amplify user expression at planetary scale. The doctrinal shield removes most liability for harmful hosted content, while recommendation systems convert high-arousal expression into advertising revenue. They can adjust incorporation jurisdiction, moderation posture, and lobbying spend; no comparable adjustment is available to the people their amplification exposes.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, platform_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Build audiences and movements through deliberately transgressive expression that carries no legal consequence short of the four categories. Deplatforming or scandal relocates them to new channels rather than ending their reach. Visibility obtained this way cannot be purchased by compliant rivals at the same price.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, political_provocateurs, beneficiary,
    moderate, biographical, mobile, national).

% Ordinary speakers whose criticism, whistle-blowing, and unpopular opinions ride the presumption of protection. The benefit arrives episodically, in the moments when they need it against officials or majorities. The same individuals can become targets of other speakers' protected expression, at which point their position inverts.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, dissenting_citizens, beneficiary,
    moderate, immediate, mobile, national).

% Individuals subjected to organized harassment campaigns, coordinated degradation, and sustained personal attacks. Almost none of the conduct reaches the narrow unprotected categories, so no remedy channel opens; courts decline the claims, and platforms decline moderation duties the law does not compel. Exit means withdrawing from public life or absorbing recurring waves of harm.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, coordinated_harassment_targets, payer,
    powerless, immediate, trapped, national).

% Communities routinely subject to demeaning, personhood-denying mass expression that the boundary classifies as protected opinion. Advocacy organizations litigate and legislate and lose under the categorical rule. Members are simultaneously protected speakers drawing on the same presumption, which fragments any unified response and complicates collective exit.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, degraded_minority_communities, payer,
    organized, biographical, constrained, national).

% Advocates and jurists pressing for standing in boundary-setting deliberation and for remedy channels for the people they represent. They enter the conversation only through amicus submissions, commentary, and losing litigation; they hold no vote in the adjudicating coalition and no seat in the doctrinal conversation that sets the threshold their constituents live under.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, target_community_representatives, excluded,
    moderate, biographical, constrained, regional).

% Study how sibling jurisdictions draw the same line differently — some subordinate expression to dignitary interests, some balance protection against demonstrated harm — and publish comparisons of downstream effects on dissent, targets, and institutional stability. They hold no stake beyond the analysis itself.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__absolutist_reading, platform_operators).
narrative_ontology:fixing_cost_class(speech_harm_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, low-discretion boundary that binds the state's censoring apparatus: by making nearly all expression presumptively protected and confining exceptions to four fixed categories, it removes case-by-case judgment about permissible speech, shields unpopular dissent and opposition organizing, and gives speakers at every resource level a predictable zone of legal safety.
% TRANSFER_FUNCTION: Moves the costs of expressive conflict — psychological injury, reputational damage, social exclusion, and withdrawal from public participation caused by protected offensive speech — onto targets without compensation, while moving communicative reach and immunity from legal consequence to speakers, concentrating usable advantage among high-volume, high-resource speakers and the intermediaries that monetize them.
% ABSENT_VOICES: Target community representatives and dignity-tradition jurists would object that remedy channels and expanded unprotected categories are owed to the people harmed by protected expression; they sit outside the adjudicating coalition, entering only as amici, commentators, and losing litigants. Harassed individuals themselves appear before the boundary-setters almost exclusively as anonymized case facts, never as seated voices.
% DISAPPEARANCE_RATIONALE: Every speaker's presumption of protection, the media and platform revenue structures built on unrestricted expression, the courts' doctrinal caseload, and dissidents' operational assumptions depend on the line staying where it is. Overnight removal would trigger an immediate litigation surge, preemptive platform takedowns, chilled political speech pending new rules, and renegotiation of the entire expressive economy.
% FOUNDING_PROBLEM: The arrangement was built to solve the censorship ratchet: governments under moral panic and incumbent pressure expand speech restriction incrementally until dissent is chilled; the founding problem was how to bind the censor's hands with a line that discretion cannot erode.
% FOUNDING_PROBLEM_CORROBORATION: Historians of sedition and censorship law — outside the beneficiary set — corroborate that the ratchet pattern the arrangement answers was real and recurrent; press-freedom monitors document continuing state pressure on expression, supporting partial liveness. Against full liveness, comparative constitutional scholarship and target-community litigation records attest that the arrangement now shelters vast volumes of commercial and provocative expression with no plausible censorship-prevention function. No single external seat attests the founding problem as simply solved or simply live; the attestation splits along the same lines as the kernel dispute itself.
narrative_ontology:disappearance_verdict(speech_harm_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_harm_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__absolutist_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__absolutist_reading_tests).
:- end_tests(speech_harm_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.38, reading-indexed: assessed by the absolutist's own lights, the arrangement is predominantly protective — the narrow categories are conceded limits, the target burden is admitted as a real but non-dominant cost, and the reading does not regard the bulk of the arrangement's operation as anyone's rent. The value is nonetheless nonzero and rising because the reading itself increasingly concedes strain: amplification economics concentrate gains while harassment industrializes the cost side. Suppression (0.62) is a raw structural property, unscaled by power or scope per the framework rule: the arrangement's persistence requires active machinery — courts declining target remedies and striking down every legislative attempt at a lower threshold — which forecloses alternative boundary regimes within this framework (though not abroad, hence accessibility_collapse at 0.48 rather than mountain-grade). Theater (0.20) is modest: the doctrine functions; the ceremonial surplus is free-speech pageantry growing slowly around a working core. Resistance (0.66) reflects sustained, losing opposition: repeated legislative attempts, academic programs, community litigation. The measurement series run on one shared grid (t=0..50 step 10) so every tracked metric is authored at every examined point; all three series end exactly at their base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   Per-seat classifications diverge sharply. From the payer seats (coordinated_harassment_targets, degraded_minority_communities), the arrangement computes as extraction-heavy: concentrated uncompensated cost, no exit, no remedy, enforcement actively foreclosing relief. From the beneficiary seats it computes as coordination: a settlement that protects their speech, revenue, and reach. The agenda-setter seat computes stewardship of a hard-won settlement it cannot leave without repudiating itself (identity_locked exit — institutional identity fusion: the court has become its interpretive tradition). Same-level lateral dynamics matter here: dissenting_citizens and coordinated_harassment_targets are nominally the same population (ordinary private individuals), differentiated purely by position in a given exchange — speaking versus targeted — which flips their exit options from mobile to trapped despite identical global standing. Inter-institutionally, apex_constitutional_court and platform_operators hold the same institutional power with opposite directionalities: the court enforces the line at escalating legitimacy cost while the platforms arbitrage it across jurisdictions. Victim-class coalition potential is structurally blunted: the degraded communities' members are also beneficiaries as speakers, so the class that would coalition against the arrangement partly staffs it — which is why resistance stays losing rather than decisive.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real structure: media corporations, platform operators, and provocateurs collect audience, revenue, and immunity; dissenting citizens collect episodic protection of their own speech. Victim declarations map likewise: harassment targets bear concentrated uncompensated costs with trapped exit; degraded communities bear diffuse chronic costs with constrained exit and internal heterogeneity. The derivation chain places powerful arbitrage beneficiaries near the beneficiary pole, powerless trapped targets near the full-target pole. One explicit override is authored: power_atom organized -> d 0.70. Without it, the victim declaration plus constrained exit drives the organized seat toward full-target d near 0.9, ignoring that community members are simultaneously subsidized speakers drawing on the same protection; the net structural relationship sits nearer 0.70. A second imperfection is documented but not overridden: the court's declared secondary beneficiary role would pull its derived d below its true position, since it chiefly administers and bears enforcement and legitimacy costs; no override is written because the override mechanism keys on power_atom and would also capture platform_operators (same institutional atom, opposite true relationship), producing a worse error than the one it fixes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — binding the censor's ratchet — is contested rather than dead: censorship pressure persists in new forms, so the arrangement has not outlived its mandate outright; but a large share of what it now protects (commercial provocation, monetized transgression) has no plausible connection to that founding problem. Classifying the arrangement as tangled_rope keeps both halves visible and prevents the two symmetrical mislabels: pure-rope labeling would erase the target extraction entirely (accepting the reading's self-presentation at face value), while snare labeling would erase the genuine anti-censorship service delivered to every eventual dissenter. Mandatrophy risk sits ahead of the story, not behind it: if platform capture consolidates further and target costs keep rising on the authored trajectories, the coordination half thins toward cover-story status and the arrangement drifts toward the snare pole — the temporal series is authored to make that gradient detectable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates only the absolutist reading of the speech_harm_boundary kernel — what changes structurally if a sibling reading is instantiated instead?',
    'Author the sibling files (speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__dignity_reading) and compare computed classifications. The disagreement is located in the boundary-setting procedure itself: categorical refusal to weigh harm against protection, versus proportional yield to demonstrated harm, versus dignity-carved exclusion.',
    'Sibling readings expand the unprotected category set and add remedy-bearing target seats, raising epsilon and pushing target seats'' effective extraction higher. The values in this file are valid only within the absolutist instantiation; averaging across readings would violate epsilon-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: this constraint is one reading of the speech-harm kernel; sibling readings are separate constraints.').

omega_variable(
    necessity_vs_construction_ambiguity,
    'Is near-absolute protection a structural necessity of any society that keeps dissent possible (as the reading''s rhetoric presents it — a quasi-natural requirement), or a constructed settlement whose shape tracks the interests of concentrated beneficiaries?',
    'Comparative constitutional history: measure whether dignity-based and balancing-based jurisdictions exhibit censorship ratchets at rates validating the necessity claim, or whether they sustain dissent while providing target remedies.',
    'If the necessity claim fails, the residual burden on targets reads as construct-sustained extraction rather than tragic coordination cost — strengthening extraction assessments and inviting false-summit-style scrutiny of the arrangement''s self-presentation as inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_vs_construction_ambiguity, conceptual, 'Whether the arrangement is a natural requirement of free dissent or a constructed settlement serving identifiable interests.').

omega_variable(
    ratchet_instability_empirical_basis,
    'Does the empirical claim beneath the reading''s first foundational axiom hold — that harm-weighing regimes cannot stably bound restriction and always ratchet toward broader censorship?',
    'Longitudinal restriction-rate data across categorical versus proportional speech regimes; institutional case studies of balancing systems that stabilized without collapsing into broad prohibition.',
    'Demonstrated stabilization anywhere undermines the first axiom, feeding axiom_overriding drift and eventually flipping the engine''s foreclosure computation toward the balancing sibling; continued ratchet evidence reinforces the absolutist framework''s coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratchet_instability_empirical_basis, empirical, 'Empirical status of the censorship-ratchet premise grounding categorical protection.').

omega_variable(
    uncompensated_target_cost_magnitude,
    'How large and how concentrated are the harm costs borne by targets of protected expression — psychological injury, reputational damage, withdrawal from public participation — relative to the coordination benefit the arrangement delivers to speakers?',
    'Survey and epidemiological measurement of participation-chilling and psychological burden among harassment-target populations; distributional analysis of who bears recurring campaign costs.',
    'High concentrated costs on powerless seats raise those seats'' computed effective extraction sharply and push per-seat classifications toward the extraction pole; diffuse small costs support the coordination framing. Resolves whether the target burden is a tolerable floor or the dominant term.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uncompensated_target_cost_magnitude, empirical, 'Magnitude and concentration of uncompensated costs borne by targets of protected speech.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__absolutist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__absolutist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t10, speech_harm_boundary__absolutist_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(spee_tr_t10, observed).
narrative_ontology:measurement(spee_tr_t20, speech_harm_boundary__absolutist_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(spee_tr_t20, observed).
narrative_ontology:measurement(spee_tr_t30, speech_harm_boundary__absolutist_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement_basis(spee_tr_t30, observed).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__absolutist_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement_basis(spee_tr_t40, observed).
narrative_ontology:measurement(spee_tr_t50, speech_harm_boundary__absolutist_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement_basis(spee_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__absolutist_reading, base_extractiveness, 0, 0.24).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t10, speech_harm_boundary__absolutist_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement_basis(spee_be_t10, observed).
narrative_ontology:measurement(spee_be_t20, speech_harm_boundary__absolutist_reading, base_extractiveness, 20, 0.29).
narrative_ontology:measurement_basis(spee_be_t20, observed).
narrative_ontology:measurement(spee_be_t30, speech_harm_boundary__absolutist_reading, base_extractiveness, 30, 0.33).
narrative_ontology:measurement_basis(spee_be_t30, observed).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__absolutist_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement_basis(spee_be_t40, observed).
narrative_ontology:measurement(spee_be_t50, speech_harm_boundary__absolutist_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement_basis(spee_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__absolutist_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t10, speech_harm_boundary__absolutist_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(spee_su_t10, observed).
narrative_ontology:measurement(spee_su_t20, speech_harm_boundary__absolutist_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(spee_su_t20, observed).
narrative_ontology:measurement(spee_su_t30, speech_harm_boundary__absolutist_reading, suppression_requirement, 30, 0.54).
narrative_ontology:measurement_basis(spee_su_t30, observed).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__absolutist_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(spee_su_t40, observed).
narrative_ontology:measurement(spee_su_t50, speech_harm_boundary__absolutist_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(spee_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__harm_balancing_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the speech-harm boundary' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints — absolutist, harm-balancing, and dignity readings of the speech_harm_boundary kernel. Each has its own epsilon (this file authors the absolutist reading's own-lights value over the fixed referent of the standing near-absolute arrangement), its own beneficiary/victim structure, and its own boundary-setting procedure. This story links both siblings via affects_constraints; the sibling files link back. Upstream/downstream: the absolutist reading is the most entrenched instantiation and functions as the baseline against which the other two define themselves, so structural pressure flows from this file toward the siblings' operating environment even though, within any single commitment framework, its axioms logically exclude theirs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_harm_boundary__absolutist_reading, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
