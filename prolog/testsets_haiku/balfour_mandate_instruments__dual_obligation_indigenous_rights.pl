% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__dual_obligation_indigenous_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__dual_obligation_indigenous_rights, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: balfour_mandate_instruments__dual_obligation_indigenous_rights
 *   human_readable: Mandate Dual Obligation: Arab Rights Protection vs. National Home Establishment
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   The Balfour Declaration (1917) and subsequent League of Nations Mandate
 *   for Palestine (1920) established a dual obligation: Britain was to
 *   facilitate the establishment of a Jewish national home while protecting
 *   the civil and political rights of the existing Arab population. This
 *   constraint story instantiates ONE reading of the contested kernel — the
 *   dual-obligation reading, which interprets the mandate instruments as
 *   imposing EQUAL OR SUPERIOR obligation to protect existing Arab rights,
 *   subordinating the national-home project to self-determination norms and
 *   minority-protection principles under international law. This reading
 *   contests the Zionist claim that the national home was the mandate's
 *   primary object. The structured extractiveness (0.72) reflects how the
 *   constraint, under this reading, operates as a tangled rope: it
 *   coordinates transition governance while imposing asymmetric costs on the
 *   Zionist project (blocked land access, capped immigration) and on the
 *   British administrator (pressed between contradictory demands). The
 *   measurement series documents the intensifying suppression requirement
 *   (0.65→0.81) as the constraint's enforcement machinery was tested and
 *   hardened against both Arab nationalist mobilization and Zionist
 *   institutional expansion. Theater rises (0.35→0.50) as performative
 *   compliance — equal-rights language without demographic protection —
 *   became the mode of British administration, especially after the
 *   mid-1930s. This reading is NOT the Zionist reading (which would invert
 *   the beneficiary/victim positions) nor the discretionary-administrator
 *   reading (which would treat the dual obligation as advisory). It is one
 *   specific interpretation, grounded in international legal precedent,
 *   covenant text, and the class-A mandate framework.
 *
 * KEY AGENTS:
 *   - palestinian_arab_communities: Trapped majority population whose tenure rights and self-determination claim structure the constraint
 *   - palestinian_arab_elites: Organized political leadership interpreting the dual obligation as ground for future sovereignty
 *   - zionist_organizations: Constrained by land-transfer restrictions and immigration quotas; bear the direct cost of the dual-obligation reading
 *   - british_mandatory_administration: Institutional bearer of the dual obligation; caught between enforcing Arab rights protection and satisfying Zionist pressure
 *   - league_of_nations_covenant: Non-agent kernel through which rights claims are adjudicated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.72).
domain_priors:suppression_score(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.81).
domain_priors:theater_ratio(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, extractiveness, 0.72).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__dual_obligation_indigenous_rights, "Mandate Dual Obligation: Arab Rights Protection vs. National Home Establishment").
narrative_ontology:topic_domain(balfour_mandate_instruments__dual_obligation_indigenous_rights, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__dual_obligation_indigenous_rights).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'b4f0c855-f700-4aac-8887-80ce1dc0728e').
narrative_ontology:cs_kernel_codification('b4f0c855-f700-4aac-8887-80ce1dc0728e', fixed_text).
narrative_ontology:cs_authority_grounding('b4f0c855-f700-4aac-8887-80ce1dc0728e', extraction).
narrative_ontology:cs_interpretation_layer_present('b4f0c855-f700-4aac-8887-80ce1dc0728e').
narrative_ontology:cs_reading_relation('b4f0c855-f700-4aac-8887-80ce1dc0728e', balfour_mandate_instruments__jewish_national_home_primacy, forecloses).
narrative_ontology:cs_reading_relation('b4f0c855-f700-4aac-8887-80ce1dc0728e', balfour_mandate_instruments__mandatory_interpretive_discretion, coexists_with).
narrative_ontology:cs_axiom('b4f0c855-f700-4aac-8887-80ce1dc0728e', foundational, equal_protection_arab_rights_mandatory).
narrative_ontology:cs_axiom_status(equal_protection_arab_rights_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('b4f0c855-f700-4aac-8887-80ce1dc0728e', equal_protection_arab_rights_mandatory, deontological).
narrative_ontology:cs_axiom('b4f0c855-f700-4aac-8887-80ce1dc0728e', foundational, arab_majority_self_determination_precedent).
narrative_ontology:cs_axiom_status(arab_majority_self_determination_precedent, holdable).
narrative_ontology:cs_axiom_grounding('b4f0c855-f700-4aac-8887-80ce1dc0728e', arab_majority_self_determination_precedent, conventional).
narrative_ontology:cs_axiom('b4f0c855-f700-4aac-8887-80ce1dc0728e', secondary, national_home_subordinate_to_existing_rights).
narrative_ontology:cs_axiom_status(national_home_subordinate_to_existing_rights, holdable).
narrative_ontology:cs_axiom_grounding('b4f0c855-f700-4aac-8887-80ce1dc0728e', national_home_subordinate_to_existing_rights, deontological).
narrative_ontology:cs_reference_frame('b4f0c855-f700-4aac-8887-80ce1dc0728e', dual_obligation_covenant_mandate).
narrative_ontology:cs_drift_state('b4f0c855-f700-4aac-8887-80ce1dc0728e', british_administrative_relaxation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b4f0c855-f700-4aac-8887-80ce1dc0728e', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Existing Arab population whose civil, political, and property rights the mandate instruments nominally protect through equality clauses and land-transfer restrictions. They inhabit and work the territory; the constraint's language secures their majority status and grounds their claim to representative governance and self-determination. They cannot exit the territory; their voice in governance is structured by the constraint's terms.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities, beneficiary,
    powerless, generational, trapped, national).

% Arab political, landowning, and intellectual leadership claiming interpretive authority over the mandate's terms. They argue that equal protection of existing rights, demographic majority, and self-determination principles make them the natural future sovereign authority. They can organize politically and litigate; they cannot unilaterally rewrite the mandate.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites, agenda_setter).

% Organizations seeking to establish a Jewish national home through land acquisition, immigration, and institutional development. The constraint (as read in this story) restricts their access to Arab-held land, caps immigration to preserve Arab majority status, and subordinates the national-home project to indigenous rights protection. They argue the mandate authorizes their project; this reading blocks that claim by judicial and textual precedent.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations, payer,
    organized, generational, constrained, global).

% The League of Nations-mandated administrative authority tasked with holding and executing the mandate's terms. Under this reading, they are bound to enforce equal protection of Arab rights and prevent demographic displacement through controlled immigration and land-transfer restrictions. They are constrained by the dual obligation; they bear the cost of refusing Zionist pressure and managing Arab political mobilization.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administration, payer,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administration, agenda_setter).

% The foundational instrument establishing the mandate system and the dual-obligation framework. Not a living actor; included as the normative kernel through which rights claims are adjudicated. The covenant's Article 22 and its class-A mandate provisions ground the equality principle this reading instantiates.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, league_of_nations_covenant, observer,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(balfour_mandate_instruments__dual_obligation_indigenous_rights, league_of_nations_covenant).

% Diaspora and international constituencies pressing for open immigration to enable demographic transformation. They are excluded from formal voice in the mandate administration; their presence outside the governed territory means they cannot participate in Arab majority governance structures. Their interests directly contradict the cap-immigration requirement this reading enforces.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, jewish_immigration_advocates, excluded,
    organized, generational, trapped, global).

% Judicial and quasi-judicial international forums that would interpret mandate obligations and adjudicate disputes. They observe the constraint as a matter of international law; their rulings would validate or invalidate readings of the mandate's dual obligation.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, international_arbitral_bodies, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__dual_obligation_indigenous_rights, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The mandate instruments establish a coordinated transition from Ottoman to post-war international governance, protecting existing inhabitants' rights while enabling a degree of Jewish institutional development — solving the coordination problem of overlapping claims to the same territory by subordinating the national-home project to indigenous rights and majority self-determination principles.
% TRANSFER_FUNCTION: The constraint restricts land acquisition from Arabs to Zionist settlers, caps immigration to preserve Arab demographic majority, and grants Arabs claim to representative governance and eventual sovereignty — transferring governance authority and property security from Zionist immigration-driven transformation to Arab majority and existing tenure holders.
% ABSENT_VOICES: Diaspora Jewish organizations and international Zionist leadership press for unconstrained immigration and land access but lack formal voice in the mandate administration. Arab minorities and tribal/clan leaders within Arab communities may dispute the elite Arab negotiating positions but are structurally excluded from international covenant adjudication.
% DISAPPEARANCE_RATIONALE: If the dual-obligation constraint and its enforcement vanished, Zionist land acquisition would accelerate beyond the constrained pace, immigration quotas would lift, and demographic composition would shift toward Jewish majority in the settled areas — reorganizing territorial control, governance authority, and property tenure within a generation. Arab political claims to future sovereignty would be substantially weakened.
% FOUNDING_PROBLEM: Post-WWI territorial reorganization required reconciling British commitments to establish a Jewish national home with existing Arab population rights and Ottoman property arrangements. The mandate system was structured to protect minority (Jewish immigrant) development while respecting the pre-existing majority (Arab) community and its claim to self-determination under international law.
% FOUNDING_PROBLEM_CORROBORATION: Arab political representatives, international legal scholars citing League of Nations precedent (class-A mandate obligations), and post-war diplomatic records from neutral powers and international law commentators attest the founding problem required dual protection. Zionist organizations and British administrators sympathetic to the national-home project argue the founding problem was primarily enabling the national home; international legal authorities outside the benefiting parties (including non-aligned international law institutes and later decolonization jurisprudence) support the dual-obligation reading as the textually and precedentially grounded interpretation.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__dual_obligation_indigenous_rights, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__dual_obligation_indigenous_rights, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the constraint asymmetrically restricts the Zionist project and forces the British to actively suppress Zionist expansion — the measured value reflects the cost imposed on the payer seats (Zionist organizations, sympathetic British administrators). Suppression is higher (0.81) than extractiveness because enforcement requires British administrative machinery to block land sales, control immigration, and prevent organized Zionist bypass. The constraint persists by active enforcement, not participant preference. Theater rises from 0.35 to 0.50 over the interval as the British increasingly resort to performative equal-rights language while de facto tolerating Zionist institutional expansion — the theater tracks the growing gap between proclaimed dual obligation and operational British policy drift. The measurement grid spans 1920–1948, capturing the mandate period from covenant entry to termination. All three metrics are authored at the same five time points (one shared grid), following the alignment rule: 1920 (projected start state), 1927 (post-Shaw Report, rising suppression), 1935 (post-Peel Commission, peak extractiveness), 1942 (WWII era, enforced stasis), 1948 (mandate end, decoupling). The rising trajectory of suppression_requirement documents the constraint's active enforcement machinery hardening as Zionist organizations pressed harder and Arab nationalism mobilized. The rising then plateauing extractiveness reflects increasing Zionist costs offset by British administrative relaxation after mid-interval.
 *
 * PERSPECTIVAL GAP:
 *   The Palestinian Arab beneficiary seats and the Zionist payer seats experience structurally opposite types from the same constraint. The Palestinian Arab reading (this one) computes the constraint as tangled_rope: genuine coordination of post-war transition, plus asymmetric cost-bearing by the national-home project. The Zionist reading (sibling constraint) would compute the same mandate texts as snare: restrictions framed as equal-rights language but operationally blocking the national-home project, no real coordination function, pure extraction toward Arab status quo. The British administrator seat sits between: they authentically coordinate transition governance (tangled_rope view) while also bearing costs from satisfying neither side fully (partial-snare experience). The engine computes per-seat divergence from the stakeholder power atoms, exit options, and beneficiary/victim declarations — the divergence is the measurement the corpus takes; it is NOT an error to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian Arab communities and elites are the structural beneficiaries (d ≈ 0.1–0.2): the constraint protects their tenure rights, guarantees their demographic majority, and grounds their self-determination claim. Zionist organizations are the structural victims (d ≈ 0.85–0.95): the constraint blocks land acquisition, caps immigration, and subordinates the national-home project to Arab majority governance. British administrators occupy an intermediate position (d ≈ 0.55–0.65): they benefit from the constraint's legitimacy as international law, but bear costs from enforcing it against Zionist pressure and managing Arab political mobilization. The directionality derivation flows from these beneficiary/victim declarations plus the stakeholder exit options: Palestinian Arabs have identity_locked and trapped exit (cannot leave the territory); Zionist organizations have constrained exit (can organize internationally but cannot unilaterally rewrite mandate terms); British administrators have mobile exit (can withdraw the mandate enforcement, though at international-law cost).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resists mandatrophy classification because the founding problem — reconciling Jewish national-home development with Arab rights protection — remains live throughout the interval. The constraint does NOT show the piton signature (atrophied function maintained theatrically). Instead, it shows rising suppression and theater as the British systematically failed to enforce the dual obligation. The theater ratio rises to 0.50 not because the constraint is functionless but because the British increasingly performed compliance (equal-rights language, consultative bodies) while operationally tolerating Zionist expansion — the performative gap widened, not the functional gap. The constraint's mandate persists because the international-law obligation remains formally alive; the British administration's de facto drift toward Zionist accommodation does not erase the constraint's existence, only the sincerity of its enforcement. The disappearance verdict (world_rearranges) confirms that the constraint is not theater: if it vanished, territorial reorganization and governance authority would shift substantively within a generation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_text_vs_intention_gap,
    'What was the League of Nations framers'' actual intention regarding the relative priority of national-home development vs. Arab rights protection? Does the mandate text itself settle this, or does the gap between text and drafting history permit both readings?',
    'Archive research on League of Nations drafting discussions, diplomatic correspondence from mandate framers (British, French, Italian, Japanese officials), and contemporaneous legal opinions. Comparison with parallel class-A mandate texts to establish precedent for how dual obligations were understood.',
    'If the historical record shows framers intended equal protection or Arab primacy, the dual-obligation reading gains foundational authority. If the record shows primacy for national-home development, the sibling jewish_national_home_primacy reading gains grounding. If the record is ambiguous or contested, the discretionary reading (mandatory_interpretive_discretion) gains strength — the constraint becomes the administrator''s power to adjudicate, not the text itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_text_vs_intention_gap, empirical, 'Historical intention regarding mandate text hierarchy').

omega_variable(
    demographic_protection_mechanism_viability,
    'Can the mandate''s immigration and land-transfer restrictions actually be enforced to maintain Arab demographic majority without becoming so restrictive they collapse the national-home project entirely, or do the two goals contain an irreducible tension that makes implementation mathematically impossible?',
    'Demographic modeling of immigration patterns, land-transfer rates, and population growth under various restriction schedules (1920s baseline). Comparison with actual British enforcement: how close did the constraint''s terms track its enforcement in practice? Analysis of the 1935–1948 period when the divergence widened.',
    'If maintenance of Arab majority is mathematically compatible with meaningful national-home development under the stated terms, the dual obligation is operationally viable and the mandate''s design is coherent. If the goals are incompatible, the constraint is structurally unsustainable and the interpretive reading that permits the administrator discretion becomes more plausible (the mandatory_interpretive_discretion reading gains ground).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_protection_mechanism_viability, empirical, 'Viability of dual-obligation enforcement under stated mechanisms').

omega_variable(
    british_administrative_capture_ambiguity,
    'To what degree did the rising theater ratio (performative compliance) reflect the constraint''s inherent unenforcibility vs. deliberate British capture by Zionist interests? Did the British honestly try to enforce dual obligation and fail due to structural tension, or did they deliberately underenforce to favor the national-home project?',
    'Analysis of British administrative records, memoranda, and enforcement decisions over 1920–1948. Comparison of stated policy (equal protection, immigration controls) with actual enforcement (land-transfer approvals, immigration permits, settlement approvals). Expert testimony from historians and administrators about decision-making rationale.',
    'If British underenforcement was deliberate and captured, the constraint is more accurately modeled as snare-with-theater (pure extraction under legitimating language) from the Palestinian Arab perspective. If underenforcement was due to structural/operational impossibility, the constraint remains tangled_rope but the suppression_requirement measurement may understate the effort cost. The distinction affects whether the rising theater_ratio reflects constraint degradation or constrained-but-genuine enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(british_administrative_capture_ambiguity, empirical, 'Intentionality and cause of British administrative drift from dual-obligation enforcement').

omega_variable(
    kernel_reading_forecast_foreclosure,
    'If one sibling reading (jewish_national_home_primacy) were formally adopted by an authoritative international tribunal, would that reading logically foreclose the dual-obligation reading, or can both readings coexist within different jurisdictional or temporal frameworks?',
    'Comparative jurisprudence from international courts and quasi-judicial bodies (Permanent Court of International Justice, later International Court of Justice) on how competing mandate readings were adjudicated. Analysis of whether multiple readings were treated as coexisting positions or as mutually exclusive options requiring authoritative selection.',
    'If a court formally adopted primacy reading and declared it the binding interpretation of the kernel, the dual-obligation reading would shift to foreclosed status (overridden by higher authority). If courts treated readings as coexisting jurisdictional or temporal positions, the dual-obligation reading remains holdable (current status). The relationship between sibling readings (coexists_with vs. forecloses) depends on this resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_forecast_foreclosure, conceptual, 'Whether rival mandate readings are mutually exclusive or coexisting positions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__dual_obligation_indigenous_rights, 1920, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1920, 0.35).
narrative_ontology:measurement_basis(balf_tr_t1920, projected).
narrative_ontology:measurement(balf_tr_t1927, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1927, 0.42).
narrative_ontology:measurement_basis(balf_tr_t1927, observed).
narrative_ontology:measurement(balf_tr_t1935, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1935, 0.48).
narrative_ontology:measurement_basis(balf_tr_t1935, observed).
narrative_ontology:measurement(balf_tr_t1942, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1942, 0.5).
narrative_ontology:measurement_basis(balf_tr_t1942, observed).
narrative_ontology:measurement(balf_tr_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1948, 0.48).
narrative_ontology:measurement_basis(balf_tr_t1948, observed).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement_basis(balf_be_t1920, projected).
narrative_ontology:measurement(balf_be_t1927, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1927, 0.68).
narrative_ontology:measurement_basis(balf_be_t1927, observed).
narrative_ontology:measurement(balf_be_t1935, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1935, 0.72).
narrative_ontology:measurement_basis(balf_be_t1935, observed).
narrative_ontology:measurement(balf_be_t1942, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1942, 0.75).
narrative_ontology:measurement_basis(balf_be_t1942, observed).
narrative_ontology:measurement(balf_be_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1948, 0.72).
narrative_ontology:measurement_basis(balf_be_t1948, observed).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1920, 0.65).
narrative_ontology:measurement_basis(balf_su_t1920, projected).
narrative_ontology:measurement(balf_su_t1927, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1927, 0.74).
narrative_ontology:measurement_basis(balf_su_t1927, observed).
narrative_ontology:measurement(balf_su_t1935, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1935, 0.79).
narrative_ontology:measurement_basis(balf_su_t1935, observed).
narrative_ontology:measurement(balf_su_t1942, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1942, 0.82).
narrative_ontology:measurement_basis(balf_su_t1942, observed).
narrative_ontology:measurement(balf_su_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1948, 0.81).
narrative_ontology:measurement_basis(balf_su_t1948, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__dual_obligation_indigenous_rights, resource_allocation).
narrative_ontology:boltzmann_floor_override(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.18).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% The balfour_mandate_instruments kernel decomposes into three constraint stories, each instantiating one reading of the contested mandate text. The dual_obligation_indigenous_rights reading (this story) interprets mandate instruments as imposing equal or superior obligation to protect Arab rights and self-determination. The jewish_national_home_primacy reading interprets the same text as directing demographic and territorial transformation toward Jewish sovereignty. The mandatory_interpretive_discretion reading frames British administrative authority to adjudicate between readings as the operative constraint, treating the mandate text itself as discretionary. Each reading has a distinct epsilon, beneficiary/victim structure, and operative mechanism. All three readings are linked via network.affects_constraints to model their shared kernel. The sibling readings are separate constraint stories (separate JSON files with their own constraint_ids), not alternative perspectives within this single story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(balfour_mandate_instruments__dual_obligation_indigenous_rights, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
