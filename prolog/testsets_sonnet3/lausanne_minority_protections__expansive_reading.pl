% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__expansive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__expansive_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: lausanne_minority_protections__expansive_reading
 *   human_readable: Lausanne Treaty Minority Protections — Expansive (Functional Continuity) Reading
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This story instantiates the expansive reading of the Lausanne Treaty's
 *   minority protection articles (1923): that the treaty froze a functional
 *   baseline of pre-1923 religious institutional self-governance —
 *   administration of courts of personal status, church/foundation property,
 *   and clergy formation through theological schools — as a continuing
 *   entitlement enforceable within Turkish domestic law. The paradigm case is
 *   the Halki (Heybeliada) Greek Orthodox seminary, closed by Turkish law
 *   since 1971 and never reopened despite decades of diplomatic
 *   representations. This reading treats that closure as an unresolved breach
 *   of a live guarantee. It is deliberately authored as ONE reading among
 *   three of a single contested kernel (lausanne_minority_protections): the
 *   restrictive_reading holds that only individual worship rights survive and
 *   institutional matters are ordinary domestic law; the guarantor_reading
 *   holds that the obligation is internationally supervised rather than
 *   self-executing domestically. Each reading is its own constraint with its
 *   own epsilon; this file does not average across them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__expansive_reading, 0.28).
domain_priors:suppression_score(lausanne_minority_protections__expansive_reading, 0.55).
domain_priors:theater_ratio(lausanne_minority_protections__expansive_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__expansive_reading, rope).
narrative_ontology:human_readable(lausanne_minority_protections__expansive_reading, "Lausanne Treaty Minority Protections — Expansive (Functional Continuity) Reading").
narrative_ontology:topic_domain(lausanne_minority_protections__expansive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__expansive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__expansive_reading, 'b71a6fcb-aa66-4ebf-a890-4c659f5f09f1').
narrative_ontology:cs_kernel_codification('b71a6fcb-aa66-4ebf-a890-4c659f5f09f1', fixed_text).
narrative_ontology:cs_authority_grounding('b71a6fcb-aa66-4ebf-a890-4c659f5f09f1', distributed).
narrative_ontology:cs_reading_relation('b71a6fcb-aa66-4ebf-a890-4c659f5f09f1', lausanne_minority_protections__restrictive_reading, forecloses).
narrative_ontology:cs_reading_relation('b71a6fcb-aa66-4ebf-a890-4c659f5f09f1', lausanne_minority_protections__guarantor_reading, coexists_with).
narrative_ontology:cs_axiom('b71a6fcb-aa66-4ebf-a890-4c659f5f09f1', foundational, institutional_form_is_the_protected_object).
narrative_ontology:cs_axiom_status(institutional_form_is_the_protected_object, holdable).
narrative_ontology:cs_axiom_grounding('b71a6fcb-aa66-4ebf-a890-4c659f5f09f1', institutional_form_is_the_protected_object, conventional).
narrative_ontology:cs_axiom('b71a6fcb-aa66-4ebf-a890-4c659f5f09f1', secondary, domestic_self_execution_suffices_for_compliance).
narrative_ontology:cs_axiom_status(domestic_self_execution_suffices_for_compliance, holdable).
narrative_ontology:cs_axiom_grounding('b71a6fcb-aa66-4ebf-a890-4c659f5f09f1', domestic_self_execution_suffices_for_compliance, conventional).
narrative_ontology:cs_reference_frame('b71a6fcb-aa66-4ebf-a890-4c659f5f09f1', id_1923_functional_baseline_preservation).
narrative_ontology:cs_drift_state('b71a6fcb-aa66-4ebf-a890-4c659f5f09f1', post_1971_seminary_closure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b71a6fcb-aa66-4ebf-a890-4c659f5f09f1', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__expansive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, greek_orthodox_patriarchate).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, armenian_apostolic_community).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, jewish_community_of_istanbul).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, minority_theological_schools).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers its own ecclesiastical affairs, seminary formation, and property under this reading's premise that Lausanne froze a functioning 1923 institutional baseline into a continuing entitlement. Depends on Turkish state forbearance and periodic diplomatic pressure to keep the Halki seminary and clergy pipeline alive; has no exit from Turkish territory without abandoning its historic seat.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, greek_orthodox_patriarchate, beneficiary,
    moderate, generational, trapped, national).

% Runs parish schools, foundations, and church property administration under the same functional-continuity logic, relying on continued recognition of pre-1923 governance forms rather than reduction to individual worship rights.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, armenian_apostolic_community, beneficiary,
    moderate, generational, trapped, national).

% Maintains community foundations, schools, and religious courts of personal status inherited from Ottoman-era arrangements; the expansive reading is what preserves this administrative apparatus rather than folding it into ordinary civil law.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, jewish_community_of_istanbul, beneficiary,
    moderate, generational, constrained, national).

% Institutions (seminaries, clergy-training academies) whose continued licensing and operation is the concrete test case of this reading — most prominently the closed Halki seminary, whose reopening the expansive reading would require.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, minority_theological_schools, beneficiary,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_non_agent(lausanne_minority_protections__expansive_reading, minority_theological_schools).

% Holds sole domestic authority to license schools, register foundations, and recognize institutional self-administration. Can accept, narrow, or refuse the expansive reading unilaterally through legislation and administrative practice, and has in fact closed the key seminary this reading would reopen. Its cooperation is what the reading's survival depends on.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, turkish_state, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Signatories (UK, France, Italy, Greece and others) with a treaty-law interest in Lausanne's minority articles but no standing enforcement mechanism inside Turkish domestic law; their diplomatic interventions are occasional and non-binding under this reading, which treats the guarantee as self-executing domestically rather than internationally supervised.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, guarantor_states, excluded,
    institutional, civilizational, constrained, continental).

% Would object that the expansive reading re-imports a millet-style parallel legal order incompatible with unitary Turkish sovereignty and the 1924 secularization reforms; this constituency's objections shape domestic legislative resistance to reopening seminaries or expanding foundation autonomy, but its voice is not part of the treaty-interpretation record.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, turkish_secular_nationalist_constituency, excluded,
    organized, generational, mobile, national).

% Study the divergence between the 1923 negotiating record, subsequent Turkish practice, and comparative minority-treaty jurisprudence to assess whether functional continuity was the treaty's intended scope or a maximalist gloss.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, internationally referenced baseline that lets long-standing religious minority communities maintain self-administered institutions (courts of personal status, foundations, schools, clergy training) without requiring case-by-case renegotiation of their legal status with each change in Turkish domestic politics.
% TRANSFER_FUNCTION: Moves recognition and institutional latitude (licensing, property administration, clergy formation) from the discretionary domestic legal order to a treaty-anchored entitlement; no material transfer of resources from one party to another is intrinsic to the reading itself — its cost is the ongoing diplomatic and administrative burden of maintaining an exception.
% ABSENT_VOICES: Turkish secular-nationalist constituencies who view functional continuity as reviving Ottoman millet privilege are not parties to the treaty-interpretation contest; guarantor states with a supervisory interest are structurally sidelined by this reading's domestic-self-execution premise, which is precisely what the guarantor_reading disputes.
% DISAPPEARANCE_RATIONALE: If the expansive reading were abandoned entirely, the Greek Orthodox Patriarchate, Armenian Apostolic community, and Jewish community foundations would lose their strongest legal basis for resisting closure or absorption of their schools, courts, and property administration into ordinary Turkish civil and administrative law — the Halki seminary's closure since 1971 would become the norm rather than the contested exception, and institutional self-administration would likely erode within a generation.
% FOUNDING_PROBLEM: At Lausanne (1923), the new Turkish Republic needed international recognition and Greece/Turkey needed a framework for population exchange and minority treatment that would not leave residual religious minorities exposed to majoritarian nation-building; the negotiators wrote minority protection articles to preserve pre-existing communal institutional life rather than dissolve it into individual civil rights alone.
% FOUNDING_PROBLEM_CORROBORATION: Minority community leaders and some international legal scholars attest the founding problem (protecting communal institutional continuity, not just individual belief) remains live and unresolved, citing the still-closed Halki seminary as evidence the guarantee is unmet. Turkish state officials and secular-nationalist legal commentary, from outside the beneficiary set, attest the founding problem was resolved by 1920s-30s minority-status settlements and that continued institutional claims exceed the treaty's original individual-rights scope — this is precisely the interpretive fault line the reading contest exists to adjudicate.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__expansive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__expansive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__expansive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lausanne_minority_protections__expansive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__expansive_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__expansive_reading_tests).
:- end_tests(lausanne_minority_protections__expansive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low-moderate (0.28) and non-extractive in structure: no party's structural position is worsened by this reading's operation viewed on its own terms — the reading claims to preserve, not transfer, communal autonomy. Suppression is measured higher (0.55) because the reading's practical survival depends on continuous diplomatic and legal pressure against a state that has, in the Halki case, exercised its domestic authority to foreclose the very institutional continuity the reading asserts; the gap between suppression and extraction is the signature of a coordination claim under active contest rather than a settled extraction mechanism. Theater ratio rose after 1971 (0.10 to 0.32) as advocacy activity (diplomatic statements, resolutions, commemorative events around Halki) increasingly substituted for the substantive result (actual seminary reopening) the reading calls for.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary communities' seat, this reading describes a living, if imperiled, coordination structure — treaty-anchored self-governance that has kept communal institutions alive for a century. From the Turkish state's seat, the same structure looks like a standing claim on domestic sovereignty that it may accept, narrow, or ignore at will, which is why its exit_options are authored as arbitrage rather than constrained. The engine's per-seat computation should reflect that asymmetry: the reading's classification is far more secure from the beneficiary seat than from the agenda_setter seat, where it depends entirely on continued forbearance.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority religious communities and their theological institutions are the structural beneficiaries under this reading — they are the parties whose institutional continuity the reading exists to protect, with low derived directionality (near the beneficiary end) because the reading's whole content is that the treaty subsidizes their self-administration. No victim group is declared: this reading does not identify any party structurally extracted from by minority self-administration itself (the guarantor_reading and restrictive_reading, by contrast, position the Turkish state's sovereign discretion or the guarantor powers' supervisory prerogative as the contested cost-bearer — those are different constraints). The Turkish state sits as agenda_setter with institutional power and arbitrage-grade exit (it can simply decline to license), which is exactly why the reading's classification is vulnerable to reversion toward piton or worse if the state's forbearance disappears entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting communal religious institutional life against majoritarian nation-building pressure in the successor states of the Ottoman Empire — is contested rather than settled: minority communities and sympathetic scholars regard it as unresolved (Halki remains closed after five decades), while Turkish domestic legal opinion regards 1920s-30s minority settlements as having discharged the obligation. This reading resists premature classification as mere legacy theater precisely because the underlying institutional stakes (an operating seminary, a functioning ecclesiastical court, a foundation's property title) remain concrete and contested rather than symbolic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_continuity_vs_frozen_privilege,
    'Does the 1923 negotiating record support ''functional continuity'' (the institutional form must keep operating indefinitely) or only a one-time recognition of the status quo at signature, with subsequent domestic law free to modify it going forward?',
    'Close reading of the Lausanne Conference travaux préparatoires and comparative analysis of how the equivalent minority articles in other post-WWI treaties (e.g., Polish Minorities Treaty) were interpreted and enforced or abandoned over time.',
    'If the travaux support only point-in-time recognition, this reading''s core premise weakens substantially toward the restrictive_reading; if they support ongoing functional guarantee, this reading''s institutional claims (e.g., Halki reopening) gain textual support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_continuity_vs_frozen_privilege, empirical, 'Whether the treaty text/history supports an ongoing functional guarantee or a one-time status recognition.').

omega_variable(
    state_forbearance_durability,
    'How durable is Turkish state forbearance toward minority institutional self-administration absent active diplomatic pressure, and what would trigger its withdrawal?',
    'Track record analysis: compare periods of eased vs. tightened administrative practice against EU accession negotiation cycles and bilateral Greek-Turkish relations to identify whether forbearance is treaty-driven or realpolitik-driven.',
    'If forbearance tracks external leverage rather than treaty obligation, the expansive reading''s classification drifts from rope toward a more fragile, enforcement-dependent structure whose persistence is contingent rather than principled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_forbearance_durability, empirical, 'Whether institutional self-administration persists on treaty principle or contingent diplomatic leverage.').

omega_variable(
    reading_choice_evidentiary_basis,
    'Under the CS-framing under-determination guidance: is the expansive reading the natural default framing of ''Lausanne minority protections,'' or does selecting it over the restrictive_reading already embed a contestable interpretive choice about what counts as the treaty''s kernel?',
    'Compare how Turkish courts, ECtHR jurisprudence (where invoked), and international legal commentary independently characterize the treaty''s operative scope, to see whether the expansive framing or the restrictive framing better matches the dominant interpretive tradition.',
    'If the restrictive_reading is in fact the dominant interpretive tradition and the expansive reading is a minority scholarly/advocacy position, this story''s beneficiary-side framing should be understood as one contested claim among several rather than the default characterization of the kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_choice_evidentiary_basis, conceptual, 'Whether the expansive framing chosen for this story is the dominant or a minority interpretive tradition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__expansive_reading, 1923, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1923, lausanne_minority_protections__expansive_reading, theater_ratio, 1923, 0.1).
narrative_ontology:measurement(laus_tr_t1940, lausanne_minority_protections__expansive_reading, theater_ratio, 1940, 0.15).
narrative_ontology:measurement(laus_tr_t1971, lausanne_minority_protections__expansive_reading, theater_ratio, 1971, 0.28).
narrative_ontology:measurement(laus_tr_t1990, lausanne_minority_protections__expansive_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(laus_tr_t2010, lausanne_minority_protections__expansive_reading, theater_ratio, 2010, 0.31).
narrative_ontology:measurement(laus_tr_t2024, lausanne_minority_protections__expansive_reading, theater_ratio, 2024, 0.32).

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__expansive_reading, base_extractiveness, 1923, 0.1).
narrative_ontology:measurement(laus_be_t1940, lausanne_minority_protections__expansive_reading, base_extractiveness, 1940, 0.15).
narrative_ontology:measurement(laus_be_t1971, lausanne_minority_protections__expansive_reading, base_extractiveness, 1971, 0.22).
narrative_ontology:measurement(laus_be_t1990, lausanne_minority_protections__expansive_reading, base_extractiveness, 1990, 0.25).
narrative_ontology:measurement(laus_be_t2010, lausanne_minority_protections__expansive_reading, base_extractiveness, 2010, 0.27).
narrative_ontology:measurement(laus_be_t2024, lausanne_minority_protections__expansive_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1923, lausanne_minority_protections__expansive_reading, suppression_requirement, 1923, 0.2).
narrative_ontology:measurement(laus_su_t1940, lausanne_minority_protections__expansive_reading, suppression_requirement, 1940, 0.3).
narrative_ontology:measurement(laus_su_t1971, lausanne_minority_protections__expansive_reading, suppression_requirement, 1971, 0.5).
narrative_ontology:measurement(laus_su_t1990, lausanne_minority_protections__expansive_reading, suppression_requirement, 1990, 0.52).
narrative_ontology:measurement(laus_su_t2010, lausanne_minority_protections__expansive_reading, suppression_requirement, 2010, 0.54).
narrative_ontology:measurement(laus_su_t2024, lausanne_minority_protections__expansive_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__expansive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__expansive_reading, 0.1).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language label 'Lausanne minority protections' per the ε-invariance principle: expansive_reading (this file, epsilon ~0.28, rope), restrictive_reading (lower institutional scope, likely mountain-adjacent domestic-law framing), and guarantor_reading (adds an international-enforcement layer, likely tangled_rope given the guarantor states' selective diplomatic engagement). The three share a kernel (the treaty text and its 1923 negotiating context) but diverge in what they claim the kernel obligates, to whom, and via what enforcement channel — hence three distinct epsilon values rather than one averaged constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
