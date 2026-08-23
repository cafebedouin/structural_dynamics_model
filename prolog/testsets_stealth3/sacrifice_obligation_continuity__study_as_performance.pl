% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__study_as_performance, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: sacrifice_obligation_continuity__study_as_performance
 *   human_readable: Study-as-Fulfillment Reading of Sacrificial Obligation Continuity
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   A post-temple religious community holds that the commandments governing
 *   sacrificial rite remain binding after the loss of the altar, and that
 *   their discharge has migrated into textual engagement: systematic study of
 *   the sacrificial legislation counts as fulfillment of the commandment
 *   itself. The constraint organizes daily and calendrical study practice
 *   around the sacrificial orders, sustains academy curricula, and keeps the
 *   community's observance account intact without any ritual site.
 *   Participation is voluntary, access requires literacy and time rather than
 *   wealth or geography, and no party bears uncompensated cost. The claimed
 *   type and the authored metrics are independent facts: the metrics describe
 *   the reading's actual gentle operation, and the engine computes per-seat
 *   classifications from the structural data.
 *
 * KEY AGENTS:
 *   - sacrifice_law_students: primary beneficiaries (moderate/mobile) — discharge the obligation through accessible textual engagement
 *   - rabbinic_academies: institutional beneficiaries (organized/constrained) — curricula gain normative weight as fulfillment
 *   - halakhic_authorities: agenda_setters (institutional/constrained) — codify and transmit the equivalence doctrine
 *   - diaspora_jewish_communities: collective beneficiaries (organized/constrained) — observance kept intact without a site
 *   - premodern_illiterate_laity: excluded seat (powerless/trapped) — fulfillment path structurally unreachable; commentary-grade only
 *   - comparative_ritual_scholars: analytical observers — document the textualization-after-site-loss pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__study_as_performance, 0.12).
domain_priors:suppression_score(sacrifice_obligation_continuity__study_as_performance, 0.06).
domain_priors:theater_ratio(sacrifice_obligation_continuity__study_as_performance, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, extractiveness, 0.12).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0.06).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__study_as_performance, "Study-as-Fulfillment Reading of Sacrificial Obligation Continuity").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__study_as_performance, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__study_as_performance, '59eb7396-b82d-4680-a6f8-ffdf9cd4f3a9').
narrative_ontology:cs_kernel_codification('59eb7396-b82d-4680-a6f8-ffdf9cd4f3a9', fixed_text).
narrative_ontology:cs_authority_grounding('59eb7396-b82d-4680-a6f8-ffdf9cd4f3a9', lineage).
narrative_ontology:cs_interpretation_layer_present('59eb7396-b82d-4680-a6f8-ffdf9cd4f3a9').
narrative_ontology:cs_reading_relation('59eb7396-b82d-4680-a6f8-ffdf9cd4f3a9', sacrifice_obligation_continuity__performance_only, forecloses).
narrative_ontology:cs_reading_relation('59eb7396-b82d-4680-a6f8-ffdf9cd4f3a9', sacrifice_obligation_continuity__messianic_suspension, forecloses).
narrative_ontology:cs_reading_relation('59eb7396-b82d-4680-a6f8-ffdf9cd4f3a9', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_axiom('59eb7396-b82d-4680-a6f8-ffdf9cd4f3a9', foundational, study_constitutes_sacrificial_fulfillment).
narrative_ontology:cs_axiom_status(study_constitutes_sacrificial_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('59eb7396-b82d-4680-a6f8-ffdf9cd4f3a9', study_constitutes_sacrificial_fulfillment, conventional).
narrative_ontology:cs_axiom('59eb7396-b82d-4680-a6f8-ffdf9cd4f3a9', foundational, obligation_binding_without_altar).
narrative_ontology:cs_axiom_status(obligation_binding_without_altar, holdable).
narrative_ontology:cs_axiom_grounding('59eb7396-b82d-4680-a6f8-ffdf9cd4f3a9', obligation_binding_without_altar, deontological).
narrative_ontology:cs_reference_frame('59eb7396-b82d-4680-a6f8-ffdf9cd4f3a9', continuous_textual_discharge).
narrative_ontology:cs_drift_state('59eb7396-b82d-4680-a6f8-ffdf9cd4f3a9', contemporary_mass_study_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('59eb7396-b82d-4680-a6f8-ffdf9cd4f3a9', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, sacrifice_law_students).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, rabbinic_academies).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, diaspora_jewish_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engages daily or weekly with the sacrificial orders of the legal corpus as a devotional practice. Under the governing doctrine this engagement discharges the sacrificial commandment itself, so the time spent returns to them as fulfilled obligation and accumulated learning. Exit is ordinary: they may reduce or stop study at any time without sanction; the cost of stopping is felt only inside their own evaluative framework.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, sacrifice_law_students, beneficiary,
    moderate, biographical, mobile, global).

% Institutions whose curricula devote substantial hours to sacrificial legislation. The equivalence doctrine gives that curricular block normative weight as commandment-fulfillment rather than mere historical study, anchoring enrollment, staffing, and donor support. The curriculum is fused with the institution's identity; leaving it would mean redefining what the institution is.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, rabbinic_academies, beneficiary,
    organized, generational, constrained, global).

% Codify, transmit, and adjudicate the ruling that textual engagement satisfies the sacrificial commandment. They determine which texts count, which study regimens qualify, and how the obligation is taught. Their office rests on the transmission chain they administer; dissolving the continuity doctrine would undercut the framework that grounds their own authority.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, halakhic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Communities with no sacrificial site that nonetheless schedule collective study of sacrifice law through synagogue cycles, printed study calendars, and household practice. The doctrine lets them treat communal observance as intact rather than broken; stepping away would mean conceding a permanent gap in the community's observance account.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, global).

% Historically the majority: laborers without literacy or leisure, for whom sustained textual study was unreachable. If study is the fulfillment, their obligation had no accessible discharge path; they appear in the record mainly through arrangements others made on their behalf, such as proxy recitation and charitable substitution. They were not seated in the interpretive process that defined fulfillment.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, premodern_illiterate_laity, excluded,
    powerless, biographical, trapped, regional).

% Document the recurring pattern of textualization after cult-site loss across religious traditions and assess whether the equivalence doctrine is functional adaptation, exegetical discovery, or institutional self-maintenance. They collect no fulfillment and bear no obligation; their seat is analytic.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, comparative_ritual_scholars, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__study_as_performance, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__study_as_performance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared legal-devotional canon across dispersed communities with no central ritual site: standardized texts, study cycles, and teacher lineages keep the sacrificial legislation uniformly known and transmitted, so scattered practitioners coordinate on the same body of law without any altar, priesthood, or geographic center.
% TRANSFER_FUNCTION: Moves time and attention from individual obligatees into the textual tradition, and moves fulfillment-status back to the learner: students surrender study-hours, academies and lineages accumulate continuity and standing, and the commandment is discharged symbolically rather than materially — no wealth, labor, or animals change hands.
% ABSENT_VOICES: Those for whom sustained study was structurally unavailable — the premodern illiterate and time-poor laity — would object that an equivalence doctrine converts their circumstance into permanent non-fulfillment; they sat outside the academies where the doctrine was formulated. Constituencies of the sibling readings (restorationists, secular archivists) also dispute the equivalence itself, but they are seated in their own constraint stories, not this one.
% DISAPPEARANCE_RATIONALE: If the equivalence doctrine and the persistent obligation vanished overnight, the sacrificial orders would migrate from devotional curricula to historical syllabi within a generation: study calendars would drop them, academies would reallocate the hours, and the communal sense of an intact observance would be replaced by an openly acknowledged gap — the archival configuration becomes the default rather than a minority position.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE), a commandment system centered on altar performance lost its performance site: the community faced either counting hundreds of commandments as permanently violated, or watching the entire sacrificial legal corpus go inert. The arrangement was built to keep the obligation alive and dischargeable without an altar.
% FOUNDING_PROBLEM_CORROBORATION: Historians of post-70 Judaism and comparative-ritual scholars corroborate the founding predicament — cult-site loss followed by large-scale textualization is independently documented across traditions. Within the framework, the classical Talmudic equations of sacrificial study with offering attest the problem's framing, but they are the benefiting parties' own record. No source outside the tradition attests the obligation's bindingness itself — that premise is internally constituted, and this story states so plainly.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__study_as_performance, 0.12, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__study_as_performance_tests).
:- end_tests(sacrifice_obligation_continuity__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12 at interval end) because the constraint's demand is met by an act participants experience as intrinsically valuable and accessible in principle: study requires time and literacy, not wealth, animals, or a central site. Suppression is near-floor (0.06): there is no enforcement machinery — no sanction for ceasing study, no barrier against rival interpretations operating in their own stories; participation runs on communal rhythm and voluntary assent. Theater is low (0.18): under this reading the engagement IS the function, so even symbolic recitation performs the declared work; the slow rise across the interval tracks memorial and calendrical study formats where the symbolic register thickens. Accessibility_collapse is 0.30: understanding the doctrine does not eliminate alternatives — competing construals remain live positions — so alternatives persist rather than collapsing. Resistance is 0.15: contestation comes from rival-reading constituencies and from the literacy-gate objection, and it is discursive rather than confrontational. Coordination type is identity_coordination because the dominant function is boundary maintenance — keeping a dispersed community constituted as one that keeps this commandment — rather than mere information transfer. The measurement series run on one shared seven-point grid (70-2026) with both tracked metrics authored at every point; suppression_requirement is deliberately not tracked because the enforcement picture is static-absent across the whole interval. Receipt surface: no named seat captures net gains — students receive their own fulfillment, academies receive attention but reciprocate instruction, authorities receive standing but bear custodial cost — hence gain_flow is the affirmative 'diffuse'. Removal would require the tradition's own authorities to dissolve the continuity doctrine that grounds their office, prohibitive against a negligible benefit, hence fixing_cost 'prohibitive'.
 *
 * PERSPECTIVAL GAP:
 *   From the student seat the arrangement computes as near-pure benefit: an accessible act discharges a demanding obligation. From the academy and authority seats it is identity-bearing coordination they administer and transmit. From the excluded illiterate-laity seat (commentary-grade only, per R3) the same doctrine reads as a gate: fulfillment defined as an act they could not perform. The engine computes these divergences from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared party sits at or near the beneficiary end of the directionality axis: students receive fulfillment for accessible effort (mobile exit pushes d toward the beneficiary pole), academies and communities receive continuity and standing, and the agenda-setting authorities administer a structure that grounds their own office. No victim group is declared because the reading defines no loser: the obligation's cost is paid voluntarily, in effort the payer experiences as the point. The excluded seat is deliberately NOT entered into beneficiaries or victims — authored absence is commentary-grade and must not drive classification (R3); its force is carried instead by the accessibility_gate omega.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an altar-less community holding performance-centered commandments — is still live, so the arrangement has not outlived its mandate and mandatrophy_resolved is false. The classification matters in both directions: the low-extraction profile is genuine coordination (accessible fulfillment, voluntary participation), not a cover story, so labeling it pure extraction would mistake devotion for predation; conversely the accessibility_gate omega keeps the coordination verdict honest — if fulfillment-gating on literacy left a materially burdened class, the structure would carry a victim set and tilt toward hybrid coordination/extraction. The restoration_counterfactual omega tracks the sunset question: whether this arrangement is steady-state or transitional is left undetermined by design rather than assumed away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexicality_of_kernel,
    'This constraint is one reading (study_as_performance) of the kernel sacrifice_obligation_continuity; which reading actually governs a given community''s practice, and how would the constraint''s structure change under a sibling reading?',
    'Survey authoritative rulings and communal curricula across jurisdictions and denominations; code which fulfillment semantics each community operationalizes in practice.',
    'Under the performance reading the obligation becomes currently unfulfillable (a victim set appears and extractiveness rises sharply); under suspension the obligation persists unsatisfied as a standing deficit; under the archival reading the constraint dissolves into heritage practice with no normative force. Epsilon and classification are indexed to the reading; cross-reading comparison of this file''s numbers is invalid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexicality_of_kernel, conceptual, 'Committer-frame indexicality: epsilon and structure are properties of this reading, not of the kernel label.').

omega_variable(
    equivalence_warrant_status,
    'Is the study-performance equivalence an exegetically settled identification within the tradition, or a crisis accommodation later rationalized as discovery?',
    'Philological and reception-history analysis of the classical derivations equating sacrificial study with offering, and of their treatment in the codes and responsa literature.',
    'If accommodation, the reading''s stability is contingent on continued site-loss and would collapse toward the performance reading upon any restoration; if settled identification, the reading is robust across counterfactuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equivalence_warrant_status, empirical, 'Whether the equivalence doctrine is load-bearing exegesis or post-hoc stabilization.').

omega_variable(
    accessibility_gate_victim_class,
    'Does defining fulfillment as sustained textual study create a materially burdened class of obligatees who cannot access the discharge path?',
    'Demographic literacy and leisure data crossed with the halakhic provision set for those unable to study (proxy recitation, charitable substitution, exemption rules); assess whether the provisions close the gap or merely manage it.',
    'A substantively closed gap keeps the pure-coordination verdict; a persistent unfulfilled-obligation class introduces a victim set and tilts the structure toward hybrid coordination/extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accessibility_gate_victim_class, empirical, 'Hidden-victim audit for the literacy-gated fulfillment path.').

omega_variable(
    restoration_counterfactual_status,
    'If a functioning altar were restored, would this reading convert study into preparation for performance, retain study as parallel fulfillment, or reveal itself as transitional?',
    'Doctrinal analysis of how the reading''s authorities treat partial or intermittent restorations of the offering rite, and of stated positions on full-restoration scenarios.',
    'If transitional, the arrangement carries an undeclared sunset character and should be re-read as temporary support; if parallel fulfillment is affirmed, the arrangement is steady-state coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_counterfactual_status, conceptual, 'Whether the arrangement is steady-state or implicitly transitional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__study_as_performance, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soc_study_fulfillment_tr_t70, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 70, 0.08).
narrative_ontology:measurement(soc_study_fulfillment_tr_t250, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 250, 0.09).
narrative_ontology:measurement(soc_study_fulfillment_tr_t600, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 600, 0.1).
narrative_ontology:measurement(soc_study_fulfillment_tr_t1200, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1200, 0.12).
narrative_ontology:measurement(soc_study_fulfillment_tr_t1800, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1800, 0.14).
narrative_ontology:measurement(soc_study_fulfillment_tr_t1948, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1948, 0.16).
narrative_ontology:measurement(soc_study_fulfillment_tr_t2026, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 2026, 0.18).

% Extraction over time
narrative_ontology:measurement(soc_study_fulfillment_be_t70, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 70, 0.24).
narrative_ontology:measurement(soc_study_fulfillment_be_t250, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 250, 0.21).
narrative_ontology:measurement(soc_study_fulfillment_be_t600, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 600, 0.19).
narrative_ontology:measurement(soc_study_fulfillment_be_t1200, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1200, 0.17).
narrative_ontology:measurement(soc_study_fulfillment_be_t1800, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(soc_study_fulfillment_be_t1948, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1948, 0.13).
narrative_ontology:measurement(soc_study_fulfillment_be_t2026, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 2026, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_continuity__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% The colloquial label 'the sacrificial obligation after the Temple' decomposes, per the epsilon-invariance principle, into four structurally distinct claims sharing one kernel (sacrifice_obligation_continuity) but differing on the obligation's current status and on study's relation to fulfillment: performance_only (binding, unfulfillable now), study_as_performance (this file: binding, discharged through study), messianic_suspension (suspended, readiness maintained), archival_preservation (not binding, memory preserved). Each carries its own epsilon, beneficiary set, and victim set; measuring one with another's observable changes epsilon because it changes the constraint. Genealogically the literalist performance reading is upstream; this reading influences the suspension reading (which adopts study-as-readiness) and the archival reading (which inherits the textual practice while dropping normativity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
