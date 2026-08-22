% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__individual_right_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: second_amendment_arms_right__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading: Pre-Existing Liberty Against Federal Infringement
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'second_amendment_arms_right'. The reading holds that the right to keep
 *   and bear arms is an individual liberty pre-existing government, and is
 *   protected against federal infringement by the Second Amendment. This
 *   reading differs structurally from two sibling readings: the
 *   collective_right_reading (the right protects state militia authority, not
 *   individual ownership outside militia context) and the
 *   civic_republican_reading (the right protects armed citizenship as
 *   prerequisite for republican self-governance, neither purely individual
 *   nor state-centered). Each reading produces a different constraint with
 *   different beneficiary/victim structures, different extractiveness
 *   profiles, and different operational effects on regulatory authority. This
 *   JSON describes ONLY the individual-right reading as a clean ε-invariant
 *   constraint. The kernel contest and alternative readings are documented in
 *   omega variables and cs_structure.reading_relations, not embedded in this
 *   constraint's metrics or type claim.
 *
 * KEY AGENTS:
 *   - individual_gun_owners — primary beneficiaries; gain constitutional shield against federal regulation
 *   - private_militia_adherents — beneficiaries with identity-locked exit; armed-citizen identity fused with the right
 *   - federal_regulatory_authority — constrained party; bears cost of restricted policy options
 *   - public_safety_constituencies — payer; bear higher injury/homicide risk under permissive regimes
 *   - state_regulatory_authority — beneficiary (retains sub-federal regulatory space)
 *   - gun_rights_advocacy_organizations — agenda-setter; frame and litigate the reading into law
 *   - Supreme Court (enforcing the reading) — agenda-setter; adjudicates compliance with the reading after Heller (2008)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, 0.62).
domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, 0.71).
domain_priors:theater_ratio(second_amendment_arms_right__individual_right_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__individual_right_reading, "Second Amendment Individual Right Reading: Pre-Existing Liberty Against Federal Infringement").
narrative_ontology:topic_domain(second_amendment_arms_right__individual_right_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__individual_right_reading, 'ca8332cb-84d6-47ae-934e-2eebc0be0645').
narrative_ontology:cs_kernel_codification('ca8332cb-84d6-47ae-934e-2eebc0be0645', fixed_text).
narrative_ontology:cs_authority_grounding('ca8332cb-84d6-47ae-934e-2eebc0be0645', lineage).
narrative_ontology:cs_interpretation_layer_present('ca8332cb-84d6-47ae-934e-2eebc0be0645').
narrative_ontology:cs_reading_relation('ca8332cb-84d6-47ae-934e-2eebc0be0645', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca8332cb-84d6-47ae-934e-2eebc0be0645', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('ca8332cb-84d6-47ae-934e-2eebc0be0645', foundational, arms_bearing_individual_pre_existing_right).
narrative_ontology:cs_axiom_status(arms_bearing_individual_pre_existing_right, holdable).
narrative_ontology:cs_axiom_grounding('ca8332cb-84d6-47ae-934e-2eebc0be0645', arms_bearing_individual_pre_existing_right, deontological).
narrative_ontology:cs_axiom('ca8332cb-84d6-47ae-934e-2eebc0be0645', foundational, federal_authority_constrained_by_individual_right).
narrative_ontology:cs_axiom_status(federal_authority_constrained_by_individual_right, holdable).
narrative_ontology:cs_axiom_grounding('ca8332cb-84d6-47ae-934e-2eebc0be0645', federal_authority_constrained_by_individual_right, conventional).
narrative_ontology:cs_reference_frame('ca8332cb-84d6-47ae-934e-2eebc0be0645', natural_rights_individual_armed_liberty).
narrative_ontology:cs_drift_state('ca8332cb-84d6-47ae-934e-2eebc0be0645', contemporary_regulatory_state_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ca8332cb-84d6-47ae-934e-2eebc0be0645', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, private_militia_adherents).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, federal_regulatory_authority).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, public_safety_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, state_regulatory_authority).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, constitutional_scholars_individual_reading).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, natural_rights_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, anti_federalism_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, possess a pre-existing constitutional right to own firearms for self-defense, hunting, and resistance to tyranny without federal license or registration. They can relocate to states with permissive regimes; federal prohibition attempts are claimed to violate their fundamental liberty. They organize politically and litigate to defend the right.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, individual_gun_owners, beneficiary,
    moderate, biographical, mobile, national).

% Interpret the right as protecting armed civil defense and resistance to federal overreach. Their political identity fuses with the armed-citizen narrative; exit would require abandoning a core identity commitment. They organize around the reading and provide advocacy infrastructure.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, private_militia_adherents, beneficiary,
    powerless, biographical, identity_locked, local).

% Operates under a reading that severely constrains its authority to regulate firearms at the federal level. The reading imposes legal barriers to the regulatory apparatus federal authority has attempted to build (licensing, registration, permitting schemes, prohibition of classes of weapons). Federal authorities bear the cost of restricted policy options and face litigation challenging enforcement of existing regulations.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, federal_regulatory_authority, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__individual_right_reading, federal_regulatory_authority, agenda_setter).

% Face higher firearm injury and homicide rates under permissive regimes justified by this reading; they bear the health, insurance, and opportunity costs of unrestricted civilian armament. Their attempts to advocate for regulation face constitutional barriers this reading erects. Exit options are limited to internal migration or accepting the higher risk profile.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, public_safety_constituencies, payer,
    organized, biographical, constrained, national).

% Retains plausible authority to regulate firearms within their borders under this reading (the right constrains federal power, not state power under most interpretations). States can adopt permissive or restrictive regimes; the reading preserves state-level regulatory space while narrowing federal capacity.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, state_regulatory_authority, beneficiary,
    institutional, generational, arbitrage, regional).

% Set the frame and propagate the individual-right reading through litigation strategy, lobbying, and messaging. They benefit from the reading's judicial adoption and organize resources to defend and extend it. They translate the reading into policy and law through strategic litigation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, gun_rights_advocacy_orgs, agenda_setter,
    organized, generational, mobile, national).

% Would argue for a different reading (collective-right or civic-republican) that permits greater federal regulation. They are constitutionally excluded from their preferred policy responses by the individual-right reading but remain in the discourse and contest the interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, gun_control_advocacy_orgs, excluded,
    organized, generational, constrained, national).

% Academic and intellectual defenders of the individual-right reading benefit in professional recognition, publication opportunity, and influence over constitutional jurisprudence when the reading is adopted. They provide the intellectual scaffolding and historical argument the reading requires.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, constitutional_scholars_individual_reading, beneficiary,
    analytical, biographical, analytical, global).

% Adjudicates disputes over whether federal or state regulations comply with the reading. The Supreme Court's adoption of the individual-right reading (District of Columbia v. Heller, 2008) gave it institutional authority to enforce the reading against federal regulation. Courts must apply the reading consistently, constraining regulatory authority.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, judiciary_enforcing_reading, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__individual_right_reading, gun_rights_advocacy_orgs).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reading coordinates a shared commitment that firearms ownership is a protected individual liberty antecedent to government. It provides a unified interpretive frame for resolving disputes over federal authority and gun owners' rights in a way that honors both individual liberty and constitutional constraint.
% TRANSFER_FUNCTION: Transfers regulatory capacity from the federal government to individual gun owners and state governments. Federal licensing, registration, and prohibition authority is transferred to the individual's claimed pre-existing right. The cost to the federal regulatory apparatus and to public-safety-oriented constituencies is constrained policy options; the benefit to gun owners is a claimed constitutional shield against federal intervention.
% ABSENT_VOICES: Gun-control advocates and public-health researchers focused on firearm injury reduction are structurally excluded: the reading pre-commits against their preferred policy responses (universal background checks, registration, restrictions on semi-automatic weapons). They remain in discourse but are constitutionally excluded by the reading from their primary regulatory goals.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and a collective-right or civic-republican reading replaced it, federal regulatory authority would expand dramatically; gun owners would lose the constitutional shield the reading provides; state regulatory regimes would face fewer constitutional barriers; and the constitutional landscape would reorganize around a different conception of arms-bearing as a collective or militia-related right rather than an individual pre-existing liberty.
% FOUNDING_PROBLEM: Protection of individual liberty against federal government overreach; specifically, preservation of the right to possess firearms for self-defense and as a check on tyranny, grounded in natural-rights doctrine and anti-federalism.
% FOUNDING_PROBLEM_CORROBORATION: Originalist constitutional scholars and gun-rights advocates attest the founding problem remains live—federal regulatory authority continues to expand, threatening the right. Gun-control advocates and public-health scholars attest the founding problem is obsolete—modern federal government poses no tyranny risk that citizen armament addresses, and regulatory scope has shifted from prohibition to permitting/registration. Independent historical scholarship on the Framing Era shows divided intent: some founders viewed the right as individual, others as militia-centered. The founding problem itself is historically contested; corroboration is split along reading lines.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_arms_right__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__individual_right_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_arms_right__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.62 because the reading establishes a legal claim that transfers regulatory capacity from federal authority to individuals and states. The transfer is extractive from the perspective of federal health/safety constituencies—they bear the cost of restricted regulatory options, but they do not choose the arrangement. Suppression is 0.71 because maintaining the reading requires active enforcement: courts must strike down conflicting federal regulations, gun-rights advocates must litigate to prevent legislative erosion, and the reading's opponents (gun-control advocates) are constitutionally excluded from their primary policy responses. Theater is 0.48 (moderate)—the reading's integrity maintenance includes genuine constitutional-fidelity arguments, but an increasing share of enforcement effort (from t=0 to t=12) goes to blocking federal regulatory measures rather than preserving the right itself, suggesting some function-drift toward pure exclusion of regulatory authority. The measurement series traces extractiveness rising from adoption (Heller, 2008, approximated t=0) through subsequent regulatory challenges and plateauing around t=12 as the Supreme Court consolidated the reading. Suppression similarly rises as litigation intensity increases and regulatory attempts intensify. Theater ratio rises as advocacy messaging (theater) grows relative to core adjudication (function), then stabilizes as the reading becomes settled doctrine.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (individual gun owners, gun-rights advocates, state regulatory authorities), this reading protects a pre-existing liberty and constrains federal overreach—they experience it as coordination. From the constrained party seat (federal regulatory authority) and the payer seats (public-safety constituencies), the reading imposes legal barriers to regulatory capacity they judge necessary for their goals—they experience it as extractive constraint. From the Supreme Court's analytical seat, the reading is an interpretation of constitutional text consistent with originalist method and historical evidence (contested though that evidence is). The engine computes these divergent directionalities from the structural data: the beneficiary seats have low d → low/negative χ (they benefit), the payer/constrained seats have high d → high χ (they bear costs), the judicial seat is analytical (d around 0.5). The reading's claimed_type is tangled_rope because it combines a genuine coordination function (unified legal framework for rights protection) with asymmetric extraction (federal authority and gun-control constituencies bear costs without choice in the arrangement).
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners are beneficiaries (d ≈ 0.1–0.2): they gain constitutional protection, face low legal barriers, and have moderate mobility (can relocate to permissive states if needed). Private militia adherents are also beneficiaries but identity-locked (d ≈ 0.15–0.25): their political identity is fused with the armed-citizen narrative; exit would require identity dissolution. Federal regulatory authority is a payer (d ≈ 0.75–0.85): they bear the cost of restricted policy options and must litigate to defend regulations against constitutional challenge; their exit is constrained by institutional duty. Public-safety constituencies are payers (d ≈ 0.70–0.80): they bear higher firearm injury and homicide rates, but exit is limited to internal migration or accepting higher risk. State regulatory authorities are beneficiaries (d ≈ 0.25–0.35): they retain sub-federal regulatory space and can arbitrage between permissive and restrictive regimes. Gun-rights advocacy organizations and the Supreme Court are agenda-setters (d ≈ 0.2–0.3): they set the frame and enforce the reading, benefiting from their structural authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protection of individual liberty against federal overreach) is contested in status—gun-rights advocates say it remains live (federal authority continues to expand), while gun-control advocates say it is obsolete (modern federal government poses no tyranny risk and regulatory authority has shifted from prohibition to licensing/registration). The disappearance verdict is world_rearranges: if the individual-right reading were replaced by a collective-right reading, the constitutional landscape would reorganize dramatically around federal regulatory authority. The constraint's mandatrophy is NOT resolved—the founding problem's status remains genuinely contested, not adjudicated by any neutral arbiter. The reading persists partly because it serves beneficiary interests (gun owners, advocacy organizations, state authorities) and partly because it embodies a defensible constitutional interpretation (originalist reading of the text). Mandatrophy would be triggered if the founding problem's status shifted decisively to dead AND the constraint persisted anyway—currently, the reading remains contestable on its merits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_status_contestation,
    'Is the founding problem (protection against federal tyranny) still live, or has it been made obsolete by modern state organization and federal constitutional limits?',
    'Comparative institutional analysis: (1) Do modern federal structures enable tyranny of a magnitude comparable to 18th-century state formation risks? (2) Can civilian armament meaningfully constrain modern federal military capacity? (3) Do constitutional limits (separation of powers, electoral accountability, judicial review) substitute for armed citizenship as a tyranny check? (4) Cross-national comparison: do nations without armed-citizenry protections show systematically higher tyranny rates?',
    'If the founding problem is live, the reading''s persistence is justified by its continued utility. If it is dead, the constraint becomes a historical artifact (piton candidate) extracting from present populations to serve a function that no longer exists. Classification would shift toward piton/theater diagnostics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_status_contestation, conceptual, 'Whether the founding problem persists or is historical.').

omega_variable(
    reading_kernel_contest_structure,
    'Does the individual-right reading logically foreclose the collective-right and civic-republican readings, or do they coexist as live interpretive positions on the same text?',
    'Textual analysis and historical hermeneutics: Can the text of the Second Amendment support all three readings, or does one reading''s core premise directly contradict another''s such that no single coherent framework could hold both?',
    'If readings foreclose each other, the constraint is one point in a binary or ternary choice set—adoption is winner-take-all. If they coexist, the constraint is one interpretation among live alternatives, and its persistence depends on institutional power (which courts, which legislatures, which constituencies dominate) rather than on logical necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest_structure, conceptual, 'Structure of the kernel contest: foreclosure vs. coexistence.').

omega_variable(
    originalism_historical_accuracy,
    'Does originalist historical scholarship accurately describe the Framers'' intent on individual vs. collective arms-bearing rights, or is the historical record ambiguous or contradictory?',
    'Systematic analysis of primary sources (Federalist Papers, state ratifying-convention debates, state constitutions, founding-era militia law, founding-era commentary on arms-bearing). Quantitative analysis of textual patterns and argumentative frames. Cross-check against collective-right and civic-republican scholars'' historical claims.',
    'If historical record supports individual-right interpretation with high confidence, the reading gains authority as a faithful reconstruction of original intent. If the record is ambiguous or contradicts originalist claims, the reading is revealed as a 20th-century reconstruction onto founding-era text, shifting classification focus to institutional power (which readings courts adopt) rather than historical fidelity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_historical_accuracy, empirical, 'Originalist historical foundation of the individual-right reading.').

omega_variable(
    public_safety_externalities_quantification,
    'What is the causal contribution of individual-right reading adoption to observed patterns in firearms injuries, homicides, and public-health costs? How much of the variance is attributable to the reading vs. to other factors (socioeconomic inequality, drug market violence, healthcare access, mental-health services)?',
    'Causal inference from natural experiments (jurisdictions adopting/abandoning permissive regimes, Heller decision effects), synthetic control analysis, instrumental-variable estimation. Separate firearm-specific injury/death from other violence. Quantify the health cost borne by public-safety constituencies.',
    'If the reading''s adoption increases firearm-related mortality at a detectable magnitude relative to counterfactual regimes, the extraction from public-safety constituencies is quantified and the payer role is hardened (they bear specific, measurable costs). If effects are minimal or confounded with other factors, extraction is diffuse and the payer role becomes weaker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_safety_externalities_quantification, empirical, 'Causal link between the reading and public-health costs borne by constrained constituencies.').

omega_variable(
    identity_lock_mechanism_private_militia,
    'For private militia adherents, is the identity-lock to the armed-citizen reading structural or internalized? Would the lock persist if external barriers (legal barriers to militia organization) were removed, or does it dissolve when isolation from reality-testing is ended?',
    'Post-exit trajectory analysis: if regulatory regime shifted to collective-right reading, would militia adherents maintain identity commitment to arms-bearing despite legal barriers, or would they gradually adopt alternative identities (survivalism, constitutional federalism, other activist channels)? Ethnographic and longitudinal survey data on identity persistence under changed regulatory conditions.',
    'If identity-lock is structural (fused with core self-concept), the constraint''s suppression is high even after legal barriers fall—the payer continues to bear costs through internal restrictions. If internalized (believed in under isolation), the lock dissolves when reality-testing increases, and suppression drops post-barrier-removal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_private_militia, empirical, 'Whether identity-lock to the armed-citizen narrative is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__individual_right_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_arms_right__individual_right_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(seco_tr_t0, observed).
narrative_ontology:measurement(seco_tr_t2, second_amendment_arms_right__individual_right_reading, theater_ratio, 2, 0.38).
narrative_ontology:measurement_basis(seco_tr_t2, observed).
narrative_ontology:measurement(seco_tr_t4, second_amendment_arms_right__individual_right_reading, theater_ratio, 4, 0.42).
narrative_ontology:measurement_basis(seco_tr_t4, observed).
narrative_ontology:measurement(seco_tr_t8, second_amendment_arms_right__individual_right_reading, theater_ratio, 8, 0.48).
narrative_ontology:measurement_basis(seco_tr_t8, observed).
narrative_ontology:measurement(seco_tr_t12, second_amendment_arms_right__individual_right_reading, theater_ratio, 12, 0.5).
narrative_ontology:measurement_basis(seco_tr_t12, observed).
narrative_ontology:measurement(seco_tr_t16, second_amendment_arms_right__individual_right_reading, theater_ratio, 16, 0.48).
narrative_ontology:measurement_basis(seco_tr_t16, projected).
narrative_ontology:measurement(seco_tr_t20, second_amendment_arms_right__individual_right_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(seco_tr_t20, projected).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_arms_right__individual_right_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(seco_be_t0, observed).
narrative_ontology:measurement(seco_be_t2, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2, 0.42).
narrative_ontology:measurement_basis(seco_be_t2, observed).
narrative_ontology:measurement(seco_be_t4, second_amendment_arms_right__individual_right_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement_basis(seco_be_t4, observed).
narrative_ontology:measurement(seco_be_t8, second_amendment_arms_right__individual_right_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(seco_be_t8, observed).
narrative_ontology:measurement(seco_be_t12, second_amendment_arms_right__individual_right_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement_basis(seco_be_t12, observed).
narrative_ontology:measurement(seco_be_t16, second_amendment_arms_right__individual_right_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement_basis(seco_be_t16, projected).
narrative_ontology:measurement(seco_be_t20, second_amendment_arms_right__individual_right_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(seco_be_t20, projected).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_arms_right__individual_right_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(seco_su_t0, observed).
narrative_ontology:measurement(seco_su_t2, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2, 0.61).
narrative_ontology:measurement_basis(seco_su_t2, observed).
narrative_ontology:measurement(seco_su_t4, second_amendment_arms_right__individual_right_reading, suppression_requirement, 4, 0.65).
narrative_ontology:measurement_basis(seco_su_t4, observed).
narrative_ontology:measurement(seco_su_t8, second_amendment_arms_right__individual_right_reading, suppression_requirement, 8, 0.71).
narrative_ontology:measurement_basis(seco_su_t8, observed).
narrative_ontology:measurement(seco_su_t12, second_amendment_arms_right__individual_right_reading, suppression_requirement, 12, 0.73).
narrative_ontology:measurement_basis(seco_su_t12, observed).
narrative_ontology:measurement(seco_su_t16, second_amendment_arms_right__individual_right_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement_basis(seco_su_t16, projected).
narrative_ontology:measurement(seco_su_t20, second_amendment_arms_right__individual_right_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(seco_su_t20, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__individual_right_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__individual_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__civic_republican_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested Second Amendment kernel. The sibling constraints (collective_right_reading and civic_republican_reading) represent alternative interpretations of the same constitutional text, each producing different beneficiary/victim structures, different extractiveness profiles, and different regulatory effects. All three readings coexist as live positions in contemporary constitutional discourse. The individual-right reading influences the other readings by setting judicial precedent (Heller, 2008) and by shaping the terms of public debate; the collective-right reading forecloses the individual-right reading's scope in jurisdictions that adopt it; the civic-republican reading coexists with the individual-right reading by occupying a middle ground on the spectrum of gun-regulation permissiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
