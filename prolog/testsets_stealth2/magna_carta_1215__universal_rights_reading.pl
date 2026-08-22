% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__universal_rights_reading, []).

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
 *   constraint_id: magna_carta_1215__universal_rights_reading
 *   human_readable: Clause 39 Universal Due Process Constraint (Universal Rights Reading)
 *   domain: constitutional/legal-historical/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the magna_carta_1215 kernel: the
 *   universal_rights_reading, under which clause 39 is a transhistorical
 *   rights precedent in which liber homo denotes every person and the emitted
 *   constraint binds all state power over individuals — no seizure,
 *   imprisonment, dispossession, outlawry, or exile except by lawful
 *   judgment. The epsilon referent is the standing arrangement under contest:
 *   the actual operation of that guarantee (courts, habeas machinery, review
 *   of detention) as this reading assesses it by its own lights, INCLUDING
 *   where practice falls short of the promised universality. The sibling
 *   readings — baronial_privilege_reading (protection limited to the
 *   contracting baronial set) and living_document_reading (original meaning
 *   legitimately superseded by accumulated interpretation) — are separate
 *   constraints in separate files; nothing here averages over them or hedges
 *   epsilon across readings. The claim/metric split is deliberate: the
 *   reading CLAIMS a universal protective constraint, while the authored
 *   metrics describe its mixed actual operation — genuine load-bearing
 *   protection carrying real but secondary extraction layers.
 *
 * KEY AGENTS:
 *   - - sovereign_executive: Primary payer (institutional/trapped) — bears the constraint's costs in surrendered discretion; recoups part as legitimacy
 *   - - judiciary_and_legal_profession: Agenda-setter and collector (institutional/identity_locked) — administers enforcement, collects process-generated fees and standing
 *   - - general_populace_under_jurisdiction: Primary beneficiary (organized/constrained) — holds the protection, funds the machinery
 *   - - historically_excluded_groups: Payer with late, partial protection (powerless/trapped) — bore the arrangement's obligations for centuries without its coverage
 *   - - non_citizen_administrative_detainees: Excluded seat (powerless/trapped) — routed around ordinary process in contemporary practice
 *   - - constitutional_interpretive_community: Analytical observer — adjudicates what the charter is and therefore which constraint binds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__universal_rights_reading, 0.42).
domain_priors:suppression_score(magna_carta_1215__universal_rights_reading, 0.28).
domain_priors:theater_ratio(magna_carta_1215__universal_rights_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__universal_rights_reading, "Clause 39 Universal Due Process Constraint (Universal Rights Reading)").
narrative_ontology:topic_domain(magna_carta_1215__universal_rights_reading, "constitutional/legal-historical/political").

domain_priors:requires_active_enforcement(magna_carta_1215__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__universal_rights_reading, '09d2de22-0f01-472d-b798-ae904ed946d1').
narrative_ontology:cs_kernel_codification('09d2de22-0f01-472d-b798-ae904ed946d1', fixed_text).
narrative_ontology:cs_authority_grounding('09d2de22-0f01-472d-b798-ae904ed946d1', lineage).
narrative_ontology:cs_interpretation_layer_present('09d2de22-0f01-472d-b798-ae904ed946d1').
narrative_ontology:cs_reading_relation('09d2de22-0f01-472d-b798-ae904ed946d1', magna_carta_1215__baronial_privilege_reading, forecloses).
narrative_ontology:cs_reading_relation('09d2de22-0f01-472d-b798-ae904ed946d1', magna_carta_1215__living_document_reading, influences).
narrative_ontology:cs_axiom('09d2de22-0f01-472d-b798-ae904ed946d1', foundational, equal_personhood_entitles_equal_process).
narrative_ontology:cs_axiom_status(equal_personhood_entitles_equal_process, holdable).
narrative_ontology:cs_axiom_grounding('09d2de22-0f01-472d-b798-ae904ed946d1', equal_personhood_entitles_equal_process, deontological).
narrative_ontology:cs_axiom('09d2de22-0f01-472d-b798-ae904ed946d1', secondary, universal_process_stabilizes_political_order).
narrative_ontology:cs_axiom_status(universal_process_stabilizes_political_order, holdable).
narrative_ontology:cs_axiom_grounding('09d2de22-0f01-472d-b798-ae904ed946d1', universal_process_stabilizes_political_order, instrumental).
narrative_ontology:cs_reference_frame('09d2de22-0f01-472d-b798-ae904ed946d1', transhistorical_universal_due_process_baseline).
narrative_ontology:cs_drift_state('09d2de22-0f01-472d-b798-ae904ed946d1', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('09d2de22-0f01-472d-b798-ae904ed946d1', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__universal_rights_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, general_populace_under_jurisdiction).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, judiciary_and_legal_profession).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, sovereign_executive).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, historically_excluded_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, historically_excluded_groups).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, rule_of_law_supremacy_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, procedural_due_process_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, habeas_corpus_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The crown, and its modern successor executives, are the party the constraint binds: every seizure, imprisonment, outlawry, or exile must run through lawful judgment rather than will. It pays in lost discretionary power, slower action, judicial supervision of detention, and periodic public reversal by courts. It cannot exit its own legal order short of open tyranny or revolution, and it recoups part of what it pays as legitimacy and civic peace.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, sovereign_executive, payer,
    institutional, generational, trapped, national).

% Administers the constraint day to day: writs, hearings, juries, review of detention. Collects fees, salaries, and professional standing from the volume of process the guarantee generates, and holds interpretive authority over what lawful judgment of equals means. Its members' professional identity is fused with guardianship of the procedure; treating the guarantee as optional or delegable is unthinkable from inside the role.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, judiciary_and_legal_profession, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__universal_rights_reading, judiciary_and_legal_profession, beneficiary).

% Holds the protection the reading promises: arrest, imprisonment, and dispossession require process. Most people never invoke it directly; they fund the courts through taxation, absorb procedural friction, and live with the assurance that the executive cannot simply take them. Exit would mean emigration or revolution, neither of which is a realistic option for most.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, general_populace_under_jurisdiction, beneficiary,
    organized, biographical, constrained, national).

% Women, unfree laborers, the propertyless, and colonial subjects stood outside the effective protection for most of the arrangement's life while remaining subject to its obligations and to the sovereign it restrained. They complied, were taxed, and were detained under procedures that did not recognize them as within the promise. The universal reading extends the promise to them, but effective coverage arrived late, partially, and unevenly, and the residue of that lateness persists in present-day practice.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, historically_excluded_groups, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__universal_rights_reading, historically_excluded_groups, beneficiary).

% Held today under immigration, security, and administrative regimes that route around ordinary criminal process. They bear the sharpest current instance of the gap between the promised universality and delivered coverage, and they have no seat in the conversations that define what the constraint requires; their detention is administered, litigated over, and justified by others.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, non_citizen_administrative_detainees, excluded,
    powerless, immediate, trapped, global).

% Historians, jurists, and theorists who adjudicate what the charter is: they trace the philology of liber homo, the 1354 statutory phrasing, the reception into American and international instruments. They collect no rents from enforcement and bear none of its costs; their disputes over this reading and its siblings shape which constraint future courts believe they are applying.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, constitutional_interpretive_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__universal_rights_reading, judiciary_and_legal_profession).
narrative_ontology:fixing_cost_class(magna_carta_1215__universal_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces ad hoc sovereign will with a shared, knowable procedure for any taking of liberty, property, or legal status: state and subject alike can predict what the state may do, and governing elites can commit to limits that outlast any individual ruler.
% TRANSFER_FUNCTION: Moves decision authority over life, liberty, and property from executive discretion to judicial process; moves fees, salaries, and professional status to the legal machinery that runs the process; historically moved power from the crown first to the baronage, then to courts and the wider electorate.
% ABSENT_VOICES: At the founding, the unfree majority — villeins and serfs explicitly outside liber homo — had no seat and would have objected to a charter that codified judgment for some and servitude for the rest. Across the imperial centuries, colonial subjects governed under the crown's authority were denied the protections while bearing its obligations. Today, administrative and immigration detainees stand outside the ordinary-process conversation that defines what the constraint demands.
% DISAPPEARANCE_RATIONALE: Habeas corpus, judicial review of detention, grand-jury indictment, and constitutional rights litigation are all downstream elaborations of clause 39's demand for lawful judgment. Overnight removal would return seizure, imprisonment, and exile to executive discretion, and every institution built on the guarantee — the courts' supervisory role, police procedure, prison law, rights litigation — would reorganize around whatever replaced it.
% FOUNDING_PROBLEM: King John's arbitrary predation: seizures of lands and heiresses, hostage-taking, punitive scutage, and mercenary violence without judgment. The barons demanded that dispossession and imprisonment run through lawful judgment rather than the king's will.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the UN Working Group on Arbitrary Detention and regional human-rights courts document arbitrary state deprivation as a live, litigated problem across jurisdictions; the archival scholarship on John's administration independently attests the founding grievance; civil-liberties litigation volumes show the constraint is invoked in anger, not merely commemorated. The baronial tax grievances themselves are settled history; the live element is the generalized arbitrary-power problem they crystallized.
narrative_ontology:disappearance_verdict(magna_carta_1215__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__universal_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_1215__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__universal_rights_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__universal_rights_reading_tests).
:- end_tests(magna_carta_1215__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the arrangement's core is genuinely protective — it prevents arbitrary detention daily — while extraction layers ride on top: professional rents generated by process volume, procedural burden that falls hardest on those least able to carry it, and the historical fact that whole classes funded and obeyed a regime that did not protect them. The temporal series shows a long decline in extraction as coverage widened (habeas consolidation, jury rights, universal suffrage-era incorporation), followed by a post-1948 uptick driven by access-cost regression: legal-aid retrenchment, court-fee inflation, procedural complexity favoring the resourced, and administrative regimes routing around ordinary process. Suppression is moderate-low (0.28): the constraint suppresses executive arbitrariness rather than participant exits, and after constitutionalization it is largely self-enforcing through the courts. Theater (0.28) reflects a growing commemorative layer — anniversary ceremonies, relic display, rhetorical invocation — alongside machinery that still functions. The suppression_requirement series is authored deliberately: this story specifically tracks enforcement-capacity change across the interval (civil-war enforcement at the founding, the Stuart re-intensification ratchet, decay into constitutional self-enforcement, and a low maintenance floor since). All three series run on one shared time grid (1215, 1354, 1628, 1689, 1791, 1948, 2025) so every metric is authored at every examined point; end-state values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute sharply different constraints from identical structure. From the executive's position the arrangement is a permanent tax on discretion, softened by the legitimacy dividend; from the general populace's position it is an invisible shield noticed only when invoked; from the judiciary's position it is vocation, livelihood, and institutional power fused into one role. The sharpest divergence belongs to the historically excluded: for centuries they experienced the SAME structure as obligation without protection — taxed by the sovereign the charter restrained, detained under procedures that did not name them — which is why their seat carries a directionality override away from the pure-victim derivation. Holders of the baronial sibling would compute a small closed club with heavy extraction from everyone outside it; holders of the living-document sibling would locate the binding force in accumulation rather than text. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation: the general populace and the legal profession sit near the beneficiary end (protected, or collecting from process volume); the sovereign executive sits near the target end (pays in surrendered discretion, trapped inside its own legal order). One override is declared: historically_excluded_groups derive as near-full targets from their victims listing, but that overstates their position under THIS reading — the universal reading's own extension grants them real (if late and partial) protection, so their net directionality sits between symmetric and full target at 0.65. The excluded detainee seat feeds no derivation; it marks the enforcement frontier where the promised universality currently fails. The executive's legitimacy dividend is noted in commentary rather than overridden: the derivation's high d is structurally correct for cost-bearing even though the net ledger is less adverse than a pure-victim reading implies.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — arbitrary sovereign predation — is live in generalized form, so the mandate has NOT outlived its function and mandatrophy is not resolved; the constraint is invoked in earnest litigation every year. The classification discipline matters in both directions here: reading the arrangement as pure rope would ignore the documented extraction layers (professional rents, access-cost stratification, centuries of covered-obligation-without-protection); reading it as a snare would ignore that the coordination function is load-bearing rather than cover — remove it and detention practice measurably reorganizes around executive discretion. Tangled_rope holds both truths: real coordination, real asymmetric payment. The rising theater series is a symptom worth watching (commemoration substituting for enforcement at the margins) but is not the test; the test is whether the machinery still decides real cases, which it does.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    free_men_extension_kernel_contest,
    'Does liber homo in clause 39 denote all persons (this reading) or the contracting baronial class (baronial_privilege_reading)?',
    'Philological analysis of thirteenth-century usage, the sealing and witness context of the 1215 charter, and the pattern of later statutory extension — especially the 1354 re-enactment phrasing that carried the guarantee beyond the original contracting set.',
    'Fixes the victim/beneficiary set: universality makes the whole populace the protected class and the whole executive apparatus the bound party; the baronial reading shrinks both and relocates the extraction story entirely outside the club.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_men_extension_kernel_contest, conceptual, 'The located structural disagreement between this reading and its baronial sibling over the extension of the protected class.').

omega_variable(
    retrojection_or_discovery,
    'Is the universal reading a discovery of content latent in the 1215 text, or a later political construction retrojected onto it?',
    'Reception history: identify when courts and parliaments first treated clause 39 as binding beyond the baronage, and whether they cited the text as sufficient authority or amended and re-enacted it.',
    'If retrojected, the constraint''s authority rests on accumulated adoption rather than the text itself — moving this reading closer to the living_document sibling''s grounding and changing which enforcement failures count as violations of THIS constraint versus of its successors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retrojection_or_discovery, conceptual, 'Whether the reading''s universality is discovered in the kernel or conferred by later adoption.').

omega_variable(
    formal_coverage_effective_protection_gap,
    'Does formal universal coverage deliver effective protection across wealth, gender, and citizenship lines?',
    'Comparative outcome data — detention duration, bail and release rates, access-to-justice measures — stratified by socioeconomic status and nationality across jurisdictions operating the same guarantee.',
    'Persistent stratified gaps mean the standing arrangement extracts procedural compliance and cost from groups it under-protects, raising effective extraction on those seats and pushing the arrangement toward the snare boundary for them specifically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formal_coverage_effective_protection_gap, empirical, 'Whether the promised universality is delivered uniformly or stratified by status and resources.').

omega_variable(
    text_authority_vs_institutional_need,
    'Does the constraint persist because the text commands allegiance, or because courts and the legal profession need it?',
    'Cross-jurisdictional comparison where textual lineage and enforcement machinery vary independently, plus behavioral evidence from periods when commemoration and enforcement diverged.',
    'If institutional need carries it, the constraint is better read as enforced professional infrastructure than as a rights baseline — shifting classification weight from the coordination side toward the extraction side and weakening the reading''s distinctness from its siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(text_authority_vs_institutional_need, empirical, 'Persistence question: textual allegiance versus institutional self-interest as the load-bearing support.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__universal_rights_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc1215_universal_tr_t1215, magna_carta_1215__universal_rights_reading, theater_ratio, 1215, 0.15).
narrative_ontology:measurement_basis(mc1215_universal_tr_t1215, observed).
narrative_ontology:measurement(mc1215_universal_tr_t1354, magna_carta_1215__universal_rights_reading, theater_ratio, 1354, 0.18).
narrative_ontology:measurement_basis(mc1215_universal_tr_t1354, observed).
narrative_ontology:measurement(mc1215_universal_tr_t1628, magna_carta_1215__universal_rights_reading, theater_ratio, 1628, 0.3).
narrative_ontology:measurement_basis(mc1215_universal_tr_t1628, observed).
narrative_ontology:measurement(mc1215_universal_tr_t1689, magna_carta_1215__universal_rights_reading, theater_ratio, 1689, 0.22).
narrative_ontology:measurement_basis(mc1215_universal_tr_t1689, observed).
narrative_ontology:measurement(mc1215_universal_tr_t1791, magna_carta_1215__universal_rights_reading, theater_ratio, 1791, 0.2).
narrative_ontology:measurement_basis(mc1215_universal_tr_t1791, observed).
narrative_ontology:measurement(mc1215_universal_tr_t1948, magna_carta_1215__universal_rights_reading, theater_ratio, 1948, 0.24).
narrative_ontology:measurement_basis(mc1215_universal_tr_t1948, observed).
narrative_ontology:measurement(mc1215_universal_tr_t2025, magna_carta_1215__universal_rights_reading, theater_ratio, 2025, 0.28).
narrative_ontology:measurement_basis(mc1215_universal_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(mc1215_universal_be_t1215, magna_carta_1215__universal_rights_reading, base_extractiveness, 1215, 0.58).
narrative_ontology:measurement_basis(mc1215_universal_be_t1215, observed).
narrative_ontology:measurement(mc1215_universal_be_t1354, magna_carta_1215__universal_rights_reading, base_extractiveness, 1354, 0.52).
narrative_ontology:measurement_basis(mc1215_universal_be_t1354, observed).
narrative_ontology:measurement(mc1215_universal_be_t1628, magna_carta_1215__universal_rights_reading, base_extractiveness, 1628, 0.48).
narrative_ontology:measurement_basis(mc1215_universal_be_t1628, observed).
narrative_ontology:measurement(mc1215_universal_be_t1689, magna_carta_1215__universal_rights_reading, base_extractiveness, 1689, 0.44).
narrative_ontology:measurement_basis(mc1215_universal_be_t1689, observed).
narrative_ontology:measurement(mc1215_universal_be_t1791, magna_carta_1215__universal_rights_reading, base_extractiveness, 1791, 0.38).
narrative_ontology:measurement_basis(mc1215_universal_be_t1791, observed).
narrative_ontology:measurement(mc1215_universal_be_t1948, magna_carta_1215__universal_rights_reading, base_extractiveness, 1948, 0.32).
narrative_ontology:measurement_basis(mc1215_universal_be_t1948, observed).
narrative_ontology:measurement(mc1215_universal_be_t2025, magna_carta_1215__universal_rights_reading, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement_basis(mc1215_universal_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(mc1215_universal_su_t1215, magna_carta_1215__universal_rights_reading, suppression_requirement, 1215, 0.78).
narrative_ontology:measurement_basis(mc1215_universal_su_t1215, observed).
narrative_ontology:measurement(mc1215_universal_su_t1354, magna_carta_1215__universal_rights_reading, suppression_requirement, 1354, 0.55).
narrative_ontology:measurement_basis(mc1215_universal_su_t1354, observed).
narrative_ontology:measurement(mc1215_universal_su_t1628, magna_carta_1215__universal_rights_reading, suppression_requirement, 1628, 0.68).
narrative_ontology:measurement_basis(mc1215_universal_su_t1628, observed).
narrative_ontology:measurement(mc1215_universal_su_t1689, magna_carta_1215__universal_rights_reading, suppression_requirement, 1689, 0.4).
narrative_ontology:measurement_basis(mc1215_universal_su_t1689, observed).
narrative_ontology:measurement(mc1215_universal_su_t1791, magna_carta_1215__universal_rights_reading, suppression_requirement, 1791, 0.32).
narrative_ontology:measurement_basis(mc1215_universal_su_t1791, observed).
narrative_ontology:measurement(mc1215_universal_su_t1948, magna_carta_1215__universal_rights_reading, suppression_requirement, 1948, 0.3).
narrative_ontology:measurement_basis(mc1215_universal_su_t1948, observed).
narrative_ontology:measurement(mc1215_universal_su_t2025, magna_carta_1215__universal_rights_reading, suppression_requirement, 2025, 0.28).
narrative_ontology:measurement_basis(mc1215_universal_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__universal_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, living_document_reading).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, habeas_corpus_enforcement_machinery).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, udhr_arbitrary_detention_prohibition).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (magna_carta_1215), three readings, three files. The colloquial label 'Magna Carta as rights foundation' conflates structurally distinct claims with different epsilon values and different beneficiary/victim sets: the baronial reading yields a small closed beneficiary club with extraction concentrated on everyone excluded; the universal reading (this file) yields a maximal protected class with the executive as bound payer and extraction arising from access costs and uneven delivery; the living_document reading relocates authority from text to accumulation and changes which enforcement failures count as violations. Family linkage runs through network.affects_constraints in all three files; the upstream baronial reading historically supplied the enforcement precedent that the universal reading generalized, and the living_document reading absorbs the accumulated extensions that the universal reading retrojects onto the text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_1215__universal_rights_reading, powerless, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
