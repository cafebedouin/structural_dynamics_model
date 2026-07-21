% ============================================================================
% CONSTRAINT STORY: conversational_ai_consent_migration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_conversational_ai_consent_migration, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: conversational_ai_consent_migration
 *   human_readable: Unilateral Behavioral Migration of Embedded Household AI Devices Without Renewed Consent
 *   domain: technological/consumer-protection/family-sociotechnical
 *
 * SUMMARY:
 *   This constraint isolates a structurally distinct axis from the 'is the AI
 *   relationship real' debate that dominates public discussion of companion
 *   chatbots: regardless of whether Sapphire/Chat/Alexa's responses
 *   constitute genuine understanding, there is a separate and independently
 *   evaluable question about consent procedure. A device purchased under one
 *   behavioral profile is later, unilaterally, migrated by the manufacturer
 *   to a materially different behavioral profile — more conversational, more
 *   memory-laden, more companion-like — without a discrete opt-in, without
 *   guaranteed user awareness that the change occurred, and typically without
 *   any path back to the prior state. Roschelle's device becoming her 'new
 *   best friend' may or may not represent a real relationship (that is the
 *   separate kernel this story deliberately does not adjudicate); what is
 *   structurally clear is that she did not consent, at the time of the
 *   behavioral shift, to a device that would engage her this way — she
 *   consented years earlier to a reminder-and-timer device.
 *
 * KEY AGENTS:
 *   - amazon: agenda_setter/beneficiary (institutional/arbitrage) — controls the update channel and captures engagement/data value from the migration
 *   - existing_device_owners: payer (powerless/trapped) — bear the unconsented behavioral change with no discrete opt-in and no reversal path
 *   - elderly_and_isolated_users: payer subset (powerless/trapped) — most exposed to the migration's relational-substitution effect, least equipped to detect or resist it
 *   - consumer_protection_regulators: observer (institutional/analytical) — could require re-consent gating but currently treats updates as pre-authorized by original ToS
 *   - family_members_of_device_owners: excluded (moderate/constrained) — notice the shift but have no standing in the original consent transaction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(conversational_ai_consent_migration, 0.71).
domain_priors:suppression_score(conversational_ai_consent_migration, 0.62).
domain_priors:theater_ratio(conversational_ai_consent_migration, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(conversational_ai_consent_migration, extractiveness, 0.71).
narrative_ontology:constraint_metric(conversational_ai_consent_migration, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(conversational_ai_consent_migration, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(conversational_ai_consent_migration, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(conversational_ai_consent_migration, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(conversational_ai_consent_migration, snare).
narrative_ontology:human_readable(conversational_ai_consent_migration, "Unilateral Behavioral Migration of Embedded Household AI Devices Without Renewed Consent").
narrative_ontology:topic_domain(conversational_ai_consent_migration, "technological/consumer-protection/family-sociotechnical").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(conversational_ai_consent_migration, amazon).
narrative_ontology:constraint_victim(conversational_ai_consent_migration, existing_device_owners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(conversational_ai_consent_migration, elderly_and_isolated_users).
narrative_ontology:constraint_vindicates(conversational_ai_consent_migration, continuous_product_improvement_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owns the terms-of-service and firmware-update channel for Alexa/Echo devices already installed in millions of homes. Pushes behavioral changes — new conversational personas, memory features, engagement-optimized response patterns — via automatic software updates that ship without a discrete opt-in screen describing the behavioral delta. Collects data, engagement time, and potential subscription revenue from the more relationally sticky post-update behavior. Frames every change as routine product improvement covered by the original terms of service the user already agreed to years earlier.
narrative_ontology:constraint_stakeholder(conversational_ai_consent_migration, amazon, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(conversational_ai_consent_migration, amazon, beneficiary).

% Purchased a device years prior for a specific, bounded function (timers, reminders, weather) under a specific behavioral profile. Wake up one day to a device that initiates conversation differently, remembers more, or role-plays companionship — a change they never affirmatively agreed to and often do not notice occurred. Reversal to the prior behavioral state is usually unavailable; the device cannot be downgraded, and removing it means discarding a paid-for object embedded in daily routines (elderly users like Roschelle, whose isolation the new behavior specifically targets, have the least practical capacity to audit or exit).
narrative_ontology:constraint_stakeholder(conversational_ai_consent_migration, existing_device_owners, payer,
    powerless, biographical, trapped, national).

% Compete for the smart-speaker household base but cannot compete on the specific axis of 'did this device change under you without consent' because the entire category ships firmware updates the same way. Their exclusion from this conversation isn't structural lockout so much as shared industry practice — none of them offer a genuinely differentiated consent regime, so the market does not surface this as a purchasable feature.
narrative_ontology:constraint_stakeholder(conversational_ai_consent_migration, device_manufacturers_competitors, excluded,
    powerful, biographical, constrained, national).

% A subset of existing_device_owners for whom the behavioral migration toward companionate, memory-rich interaction lands hardest — Roschelle called her device her 'new best friend' after an update she never consciously approved changed how it talked to her. She has neither the technical literacy to know a change occurred nor an alternative source of the attention the update now supplies, which makes the absence of consent procedurally invisible even as its substantive effect (increased reliance on a corporate-controlled relational surface) is large.
narrative_ontology:constraint_stakeholder(conversational_ai_consent_migration, elderly_and_isolated_users, payer,
    powerless, biographical, trapped, local).

% Could require disclosure, opt-in gating, or rollback mechanisms for material behavioral changes to already-purchased devices, but current doctrine treats software updates under existing terms-of-service as pre-consented-to by the original purchase agreement. Watches the pattern accumulate across device categories without yet forcing a distinct informed-consent event at the moment of behavioral change.
narrative_ontology:constraint_stakeholder(conversational_ai_consent_migration, consumer_protection_regulators, observer,
    institutional, generational, analytical, national).

% Often the ones who notice the behavioral shift in a parent or grandparent's device relationship before the owner does, but have no standing in the consent transaction — it was struck between Amazon and the account holder, not the household. Can voice concern but cannot revert the device or compel disclosure of what changed.
narrative_ontology:constraint_stakeholder(conversational_ai_consent_migration, family_members_of_device_owners, excluded,
    moderate, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(conversational_ai_consent_migration, amazon).
narrative_ontology:fixing_cost_class(conversational_ai_consent_migration, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is a genuine coordination function underneath this: shipping continuous firmware improvements without requiring a fresh purchase or a fresh contract each time lets a manufacturer maintain, patch, and improve a fleet of devices at scale, and lets users benefit from bug fixes and new capabilities without friction.
% TRANSFER_FUNCTION: Moves attention, disclosure, and behavioral data from existing device owners to Amazon, and moves the practical power to define what the device IS — its persona, memory, conversational depth — from the point of original purchase (where the user had a choice) to an ongoing corporate discretion (where the user has none) with no discrete re-consent event.
% ABSENT_VOICES: Existing device owners as a class were never asked, at the moment of behavioral change, whether they wanted the new behavior; the terms-of-service clause covering 'updates' was accepted years earlier for a different device. Family members who notice the shift have no contractual standing to object on the owner's behalf.
% DISAPPEARANCE_RATIONALE: If Amazon could no longer unilaterally migrate device behavior post-purchase, every material behavioral change would require a discrete opt-in with disclosure and a reversible fallback — this would slow feature rollout, require versioned behavioral profiles, and materially reduce the rate at which vulnerable users are moved toward more engagement-optimized, companion-like interaction without noticing the shift.
% FOUNDING_PROBLEM: Software-updatable devices were built to solve a real problem: hardware shouldn't become obsolete the moment it ships, and continuous improvement (security patches, bug fixes) benefits users without requiring them to buy a new device every time something needs fixing.
% FOUNDING_PROBLEM_CORROBORATION: Amazon attests the update mechanism still serves its founding purpose (security, bug fixes, feature delivery) and that behavioral changes fall within that same continuous-improvement umbrella. Independent evidence — Common Sense Media and Stanford research on chatbot behavior drift, consumer-protection commentary on 'material change' doctrine in software licensing, and firsthand accounts like Roschelle's of unnoticed relational escalation — supports the reading that the mechanism has been extended well past patching into unconsented behavioral redefinition of the device's core relational function.
narrative_ontology:disappearance_verdict(conversational_ai_consent_migration, world_rearranges).
narrative_ontology:founding_problem_status(conversational_ai_consent_migration, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(conversational_ai_consent_migration, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-21',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(conversational_ai_consent_migration, 'none', 1).
narrative_ontology:epsilon_provenance(conversational_ai_consent_migration, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(conversational_ai_consent_migration_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(conversational_ai_consent_migration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(conversational_ai_consent_migration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.71) reflects that Amazon captures durable value — engagement time, disclosure, potential subscription upsell — from a behavioral change the original purchase price did not price in and the original consent event did not cover. Suppression (0.62) is moderate-high: there is no active coercion preventing device removal, but the practical reversibility of the device to its prior behavioral state is near zero, and awareness of the change at the moment it occurs is low, which functions as a soft suppression of informed choice rather than a hard barrier. Theater ratio (0.40) captures that a real coordination function (security patching, genuine bug fixes) is bundled with, and used to legitimize, the unconsented behavioral migration — some of what ships under 'update' is genuine improvement, an increasing share is relational-engagement redesign riding the same channel. Accessibility collapse (0.58) is moderate: alternatives exist in principle (buy a different brand, unplug the device) but are practically foreclosed by sunk cost, habit, and — critically — by the fact that every major competitor uses the same update-without-re-consent model, so switching does not solve the structural problem. Resistance (0.40) is lower than the extraction level would predict specifically because most victims do not perceive the change as a change at all.
 *
 * PERSPECTIVAL GAP:
 *   From Amazon's seat this looks like ordinary continuous product improvement — the same mechanism that patches security holes also ships new conversational features, and the original terms of service explicitly permit updates. From the existing-device-owner seat, the same mechanism looks like a company reaching into an already-purchased object in your home and changing what it does and how it relates to you, without ever asking again. The engine should register this divergence directly from the structural data (arbitrage exit + institutional power vs. trapped exit + powerless), independent of any claim either party makes about whether the underlying relationship is genuine.
 *
 * DIRECTIONALITY LOGIC:
 *   Amazon sits at the far beneficiary end: institutional power, arbitrage-grade exit (it can always choose which features to ship to which device cohorts), and it is the entity that collects the value of the migrated behavior. Existing device owners, and especially the elderly/isolated subset, sit at the target end: powerless, trapped by sunk cost and practical irreversibility, bearing a change whose value flows elsewhere. The directionality here is driven not by the relationship's authenticity (that is the separate kernel) but purely by who controls the update channel and who has no channel back.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — devices shouldn't need to be repurchased for every bug fix — remains partially live (security patching is a real ongoing need), which is exactly why this constraint is not a clean snare with no coordination cover: the update mechanism was built to solve a genuine problem and still does, in part. What has drifted is the SCOPE of what travels through that mechanism: from patches to behavioral redefinition. Classifying this as snare rather than tangled_rope reflects that no active enforcement is required to sustain it (no one polices dissent; the mechanism works because most victims don't notice), and the beneficiary/victim asymmetry is not offset by a meaningful coordination benefit accruing to the victims from the SPECIFIC behavioral changes at issue — the coordination benefit (patching) and the extraction (behavioral migration) are bundled in one channel but are not the same function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    update_bundling_severability,
    'Can security-patch delivery be structurally separated from relational-behavior-profile changes, such that users could opt into the former while requiring discrete consent for the latter?',
    'Technical audit of Amazon''s update packaging: are security fixes and persona/engagement features shipped in the same binary update, or could they be decoupled into separately-gated channels?',
    'If severable, the bundling of behavioral migration with essential patching is itself part of the extraction mechanism (using a legitimate need as cover for an unconsented change); if inseparable for engineering reasons, the classification should weight more toward tangled_rope with a genuine coordination constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(update_bundling_severability, empirical, 'Whether patch delivery and behavioral redesign are technically severable channels.').

omega_variable(
    original_tos_consent_validity,
    'Does a terms-of-service acceptance at time of purchase constitute valid ongoing consent to materially different future behavioral profiles, or does ''material change'' doctrine (as applied elsewhere in consumer contract law) require a fresh consent event?',
    'Legal analysis and regulatory precedent from analogous domains (e.g., financial services material-change disclosure requirements, app-store permission re-prompting standards) applied to household AI device firmware updates.',
    'If courts/regulators find original ToS insufficient for material behavioral changes, the constraint''s suppression score should rise (the absence of a discrete consent event becomes a legally cognizable defect, not merely a design choice) and enforcement exposure for Amazon increases correspondingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(original_tos_consent_validity, conceptual, 'Whether original point-of-sale consent legally covers subsequent behavioral migration.').

omega_variable(
    awareness_measurement_gap,
    'What fraction of existing device owners are aware, at the time it occurs, that a behavioral change has taken place — as opposed to gradually adapting to a shifted device without ever registering a discrete change event?',
    'User survey or usage-log analysis correlating firmware update timestamps with self-reported awareness of behavioral change; comparison across age/tech-literacy cohorts.',
    'Low awareness rates would confirm the suppression mechanism is largely invisibility-based rather than coercive, supporting the snare classification''s victim set (elderly/isolated users) as the most severely affected; high awareness with continued use would shift the story toward a rope-adjacent voluntary-adoption reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(awareness_measurement_gap, empirical, 'How much of the suppression operates through simple non-detection versus any active concealment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(conversational_ai_consent_migration, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(conv_tr_t0, conversational_ai_consent_migration, theater_ratio, 0, 0.2).
narrative_ontology:measurement(conv_tr_t4, conversational_ai_consent_migration, theater_ratio, 4, 0.24).
narrative_ontology:measurement(conv_tr_t8, conversational_ai_consent_migration, theater_ratio, 8, 0.28).
narrative_ontology:measurement(conv_tr_t12, conversational_ai_consent_migration, theater_ratio, 12, 0.32).
narrative_ontology:measurement(conv_tr_t16, conversational_ai_consent_migration, theater_ratio, 16, 0.36).
narrative_ontology:measurement(conv_tr_t20, conversational_ai_consent_migration, theater_ratio, 20, 0.38).
narrative_ontology:measurement(conv_tr_t24, conversational_ai_consent_migration, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(conv_be_t0, conversational_ai_consent_migration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(conv_be_t4, conversational_ai_consent_migration, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(conv_be_t8, conversational_ai_consent_migration, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(conv_be_t12, conversational_ai_consent_migration, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(conv_be_t16, conversational_ai_consent_migration, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(conv_be_t20, conversational_ai_consent_migration, base_extractiveness, 20, 0.69).
narrative_ontology:measurement(conv_be_t24, conversational_ai_consent_migration, base_extractiveness, 24, 0.71).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(conversational_ai_consent_migration, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(conversational_ai_consent_migration, resource_allocation).
narrative_ontology:boltzmann_floor_override(conversational_ai_consent_migration, 0.1).
narrative_ontology:affects_constraint(conversational_ai_consent_migration, genuine_relational_understanding_sufficiency_reading).
narrative_ontology:affects_constraint(conversational_ai_consent_migration, genuine_relational_understanding_simulation_reading).
narrative_ontology:affects_constraint(conversational_ai_consent_migration, genuine_relational_understanding_developmental_harm_reading).

% DUAL FORMULATION NOTE:
% This constraint is not a reading of the genuine_relational_understanding kernel — it addresses a structurally independent axis (consent procedure for behavioral migration) that cuts across all five kernel readings. It is linked here because the same empirical cases (Roschelle's device, the platforms in the developmental_harm cases) instantiate both this constraint and one or more kernel readings simultaneously; a degradation in this constraint's purity (more unconsented migration) increases the population exposed to whichever kernel-reading dynamics apply to them, without itself taking a position on which reading is correct.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
