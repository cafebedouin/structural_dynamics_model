% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_archive, []).

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
 *   constraint_id: kodashim_obligation__study_as_archive
 *   human_readable: Kodashim Study as Archival Preservation (Defunct-Temple Reading)
 *   domain: religious/textual_preservation
 *
 * SUMMARY:
 *   The Kodashim order — sacrificial law — occupies roughly a third of the
 *   Babylonian Talmud, and the standard advanced curriculum places it at the
 *   intellectual summit even though the system it documents has been
 *   inoperative since the destruction of the Second Temple. This story
 *   instantiates the ARCHIVE reading of that practice: study persists as
 *   historical preservation and identity-maintenance; it confers no operative
 *   legal capacity, enacts no cosmic function, and is not aimed at a future
 *   restoration, which this reading treats as structurally impossible and
 *   undesired by its holders. The epsilon referent is the standing curricular
 *   arrangement itself as this reading assesses it — the expectation that
 *   advanced students master the sacrificial corpus — never the reallocated
 *   curriculum a reform program would create. The claim and the metrics are
 *   independent authored facts: tangled_rope is what this reading believes is
 *   structurally true (genuine preservation-and-identity coordination plus
 *   real asymmetric diversion of scholarly capacity), and the metrics
 *   describe the arrangement's actual operation; the engine computes per-seat
 *   classifications from the structural data, and divergence between claim
 *   and computation is signal, not error. KEY AGENTS (by structural
 *   relationship): - rabbinic_academies: Agenda-setter and principal
 *   beneficiary (institutional / identity_locked) — runs the curriculum;
 *   prestige and funding accrue to it. - advanced_yeshiva_students: Primary
 *   target (moderate / constrained) — prime learning hours diverted. -
 *   kollel_scholars: Secondary target (moderate / identity_locked) —
 *   decade-deep sunk specialization, poor transfer. - diaspora_communities:
 *   Beneficiary (organized / generational) — collects identity cohesion, pays
 *   diffusely. - practical_halakha_advocates: Excluded voice (organized /
 *   mobile) — would redirect hours, holds no curricular seat. -
 *   academic_jewish_studies_scholars: Analytical observer — sees the full
 *   historical structure from outside.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, 0.45).
domain_priors:suppression_score(kodashim_obligation__study_as_archive, 0.3).
domain_priors:theater_ratio(kodashim_obligation__study_as_archive, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, extractiveness, 0.45).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_archive, "Kodashim Study as Archival Preservation (Defunct-Temple Reading)").
narrative_ontology:topic_domain(kodashim_obligation__study_as_archive, "religious/textual_preservation").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_archive, '25db84e4-c5cf-4469-a650-f5245da3827a').
narrative_ontology:cs_kernel_codification('25db84e4-c5cf-4469-a650-f5245da3827a', fixed_text).
narrative_ontology:cs_authority_grounding('25db84e4-c5cf-4469-a650-f5245da3827a', lineage).
narrative_ontology:cs_interpretation_layer_present('25db84e4-c5cf-4469-a650-f5245da3827a').
narrative_ontology:cs_reading_relation('25db84e4-c5cf-4469-a650-f5245da3827a', kodashim_obligation__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('25db84e4-c5cf-4469-a650-f5245da3827a', kodashim_obligation__study_as_preparation, forecloses).
narrative_ontology:cs_axiom('25db84e4-c5cf-4469-a650-f5245da3827a', foundational, study_value_is_terminal_record_not_future_use).
narrative_ontology:cs_axiom_status(study_value_is_terminal_record_not_future_use, holdable).
narrative_ontology:cs_axiom_grounding('25db84e4-c5cf-4469-a650-f5245da3827a', study_value_is_terminal_record_not_future_use, empirically_contingent).
narrative_ontology:cs_axiom('25db84e4-c5cf-4469-a650-f5245da3827a', foundational, defunct_cult_procedures_impose_no_binding_study_duty).
narrative_ontology:cs_axiom_status(defunct_cult_procedures_impose_no_binding_study_duty, holdable).
narrative_ontology:cs_axiom_grounding('25db84e4-c5cf-4469-a650-f5245da3827a', defunct_cult_procedures_impose_no_binding_study_duty, conventional).
narrative_ontology:cs_reference_frame('25db84e4-c5cf-4469-a650-f5245da3827a', closed_documentary_corpus_of_defunct_temple_cult).
narrative_ontology:cs_drift_state('25db84e4-c5cf-4469-a650-f5245da3827a', contemporary_mass_study_movements, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('25db84e4-c5cf-4469-a650-f5245da3827a', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_archive, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, rabbinic_academies).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, diaspora_communities).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, advanced_yeshiva_students).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, kollel_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, kollel_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and guard the advanced curriculum across the yeshiva world. Their authority, enrollment, and faculty standing depend on demonstrating command of the full Talmudic corpus, of which the sacrificial order is the most demanding large section. Funding lines and matchmaking reputational capital flow through institutions that keep the full corpus at the center. Reallocating hours away from the sacrificial order would undercut the very mastery on which their standing rests, so no governing council treats simplification as a live option.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, rabbinic_academies, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_archive, rabbinic_academies, beneficiary).

% Spend their prime learning years on tractates covering animal offerings, meal offerings, and Temple architecture that no court will ever apply. Part of the hours return value as rigorous method-training and communal belonging; the remainder builds expertise with no object of application. Moving to a civil-law or pastoral track is possible but carries visible status penalties in the marriage market and in selection for teaching posts.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, advanced_yeshiva_students, payer,
    moderate, biographical, constrained, global).

% Post-marriage scholars supported by communal stipends to continue full-time study. A decade or more of sunk specialization in sacrificial dialectic defines their professional identity and their employability within the community's teaching economy. Outside that economy the accumulated skill transfers poorly, so mid-career redirection is rare even where privately doubted. They also receive stipends and standing from the same arrangement that consumes their hours.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, kollel_scholars, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_archive, kollel_scholars, beneficiary).

% Scattered congregations sustain the academies through donations and tuition and collect the returns: a continuous textual tradition that has held dispersed communities together for two millennia, a shared curriculum recognizable in every continent, and completion ceremonies that anchor communal calendars. They pay through the opportunity cost embedded in the scholars they fund and rarely audit what the funded hours produce.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, diaspora_communities, beneficiary,
    organized, generational, constrained, continental).

% Decisors, educators, and rabbinical-court trainees who argue that the same hours spent on damages, marriage, divorce, and medical ethics would yield immediately usable rulings, including on questions the community urgently faces. They publish curricular proposals and run small pilot programs but hold no seat in the councils where the standard course of study is set.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, practical_halakha_advocates, excluded,
    organized, biographical, mobile, global).

% Historians and philologists outside the academy system who document how the sacrificial corpus was compiled, transmitted, and repurposed after the destruction of the Temple. They describe the study practice's preservation and identity functions and its separation from any operative legal system. Their accounts circulate freely but do not touch curricular decisions.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, academic_jewish_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_archive, rabbinic_academies).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_archive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains unbroken transmission of the largest and most demanding order of the Babylonian Talmud across twenty centuries of dispersal; gives scattered communities one shared curriculum, one ladder of prestige, and one calendar of completion ceremonies; keeps the textual archive accurate through continuous recitation and argument.
% TRANSFER_FUNCTION: Moves the scarcest input in the system — advanced scholarly hours — away from directly applicable law (damages, marriage, divorce, commerce, medicine) and toward the sacrificial corpus; moves stipends, donations, and reputational capital toward the institutions and masters who certify total-corpus command.
% ABSENT_VOICES: Practical-halakha prioritists (decisors and court trainees who would redirect the hours to currently litigated questions), would-be rabbinical judges who lack a training pipeline oriented to live casework, and secular Jewish historians whose account of how the corpus was repurposed is absent from curriculum deliberations. They sit outside the councils where the standard course of study is fixed; their proposals surface in Modern Orthodox and academic venues and largely stop at the boundary of the mainstream academy system.
% DISAPPEARANCE_RATIONALE: If advanced study of the sacrificial order ceased overnight, thousands of scholar-years would redirect toward applicable law within a generation, the prestige ladder built on total-corpus mastery would collapse and rebuild around different tractates, completion-celebration cycles would lose their hardest object, and the methodological schools organized around sacrificial dialectic would shrink to historical footnotes. The printed texts would survive untouched, so the archive itself persists; the living practice economy built on studying it is what rearranges.
% FOUNDING_PROBLEM: Preserve exact procedural knowledge of the sacrificial system — first so the Temple service could be performed correctly while it stood, and after 70 CE so that a restored service could someday resume without a break in transmission.
% FOUNDING_PROBLEM_CORROBORATION: No consuming institution calls for the knowledge: the Israeli Chief Rabbinate certifies judges on marital and commercial law, not sacrificial procedure, and rabbinical courts adjudicate with no recourse to offering rules — attestation from outside the benefiting parties that the resumption-preparation problem is dormant at best. Academic histories of the canon, written entirely outside the beneficiary set, document the corpus's conversion from performance manual to scholastic object after the destruction. Within the tradition, the classical sources themselves (the Talmud's statement that study of the laws stands in for the service, Menahot 110a) corroborate the substitution logic, though that witness is internal to the arrangement being classified.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_archive, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_archive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_archive, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_archive_tests).
:- end_tests(kodashim_obligation__study_as_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45): the arrangement diverts the community's scarcest input — advanced scholarly capacity — from directly applicable law into a corpus with zero operative output, but the diversion is bounded by genuine preservation value, real method-training returns, and largely voluntary participation. Suppression is authored as a raw structural property and is NOT scaled by power or scope (only extractiveness scales): at 0.30 it captures social-normative enforcement — track-switching stigma, marriage-market penalties, funding and appointment gatekeeping — not physical coercion. Theater is moderate-low (0.25): completion ceremonies and sanctity-framing exceed the informational function, but the archival scholarship underneath is real work. Accessibility collapse is low (0.20): practical-law tracks, academic study, and exit remain structurally open; nothing is foreclosed by understanding the system is defunct. Resistance is modest (0.25): recurring reform currents (musar-era critiques, Modern Orthodox redirections toward applicability) surface periodically but never sustain, because the identity rewards of corpus mastery are real. The temporal series run on one shared grid (1000, 1200, 1450, 1700, 1900, 2025) so every metric is authored at every examined point. The 1450 dip in extractiveness and theater reflects expulsion-era institutional disruption, not cyclical reinforcement; there is no strong oscillator here, only slow drift and one exogenous shock. The suppression series is deliberately falling: enforcement migrated from coercive communal instruments (medieval excommunication powers over deviant study) to identity-based retention after emancipation removed the coercion — the constraint now persists on internalized commitment rather than enforceable sanction, which is why suppression decays while extractiveness holds steady.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently: from the academies' position the arrangement is the tradition they embody and transmit; from the diverted student seats it consumes irreplaceable years. Two same-level payer seats diverge on exit: advanced_yeshiva_students and kollel_scholars hold the same power atom (moderate), but students retain constrained track-switching while kollel scholars are identity_locked by sunk specialization — the engine should differentiate their effective positions from exit alone. Inter-institutionally, the academies, the community funders, and the state certification bodies experience the arrangement differently: funders pay without auditing, certifying bodies ignore the sacrificial corpus entirely when licensing judges, and only the academy seat is bound by the mastery norm. The observer seat sees the substitution history the participant seats do not.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to position as follows: rabbinic_academies are agenda-setter plus declared beneficiary — the derivation reads near-full-beneficiary, but the override raises institutional d to 0.15 because the administrators are legitimacy-captive: they cannot shed the corpus requirement without dissolving the mastery on which their own authority rests, so they sit slightly above pure beneficiary. diaspora_communities are declared beneficiaries with constrained exit — they collect identity cohesion and pay only diffusely through funded hours, sitting low but not at zero. advanced_yeshiva_students and kollel_scholars are declared victims; constrained versus identity_locked exit places the students somewhat below the scholars on the target scale. practical_halakha_advocates carry no beneficiary or victim declaration — they are outside the arrangement's flow, and the derivation handles them from their exclusion position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — preserve capacity for a resumable service — is dead under this reading, and the arrangement persists on a successor mandate (transmission, identity, prestige) that was never the reason it was built. That mismatch is authored openly: founding_problem_status=dead against disappearance_verdict=world_rearranges is the honest genealogy, and the resulting capture/zombie flag correctly routes investigation toward which mandate is doing the persistence work. The tangled_rope claim is what prevents mislabeling in both directions: calling the arrangement a snare erases the genuine preservation and identity function that two millennia of diaspora continuity attest; calling it a rope erases the real asymmetric cost borne by diverted scholars for the standing of administrators. Mandatrophy resolution here is a property of the mandate, not a metric: the flag was authored from the genealogy interview, independent of the theater ratio.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (study_as_archive) of the kodashim_obligation kernel; would instantiating a sibling reading change the structural data and the classification?',
    'Comparative re-authoring under each sibling: under study_as_performance the diverted-hours victim set empties (the hours ARE the point) and epsilon falls toward the coordination floor; under study_as_preparation a binding obligation attaches, raising suppression and extractiveness and shifting beneficiaries toward messianic-readiness institutions.',
    'Classification swings materially across readings — rope-adjacent under performance, snare-adjacent under preparation, tangled_rope under archive. Per-reading stories must stay epsilon-invariant; cross-reading comparison is valid only between files, never inside one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: which reading of the kernel this story instantiates and how siblings would reshape the victim and beneficiary sets.').

omega_variable(
    temple_restoration_contingency,
    'Is restoration of the sacrificial system permanently foreclosed — making the archive frame stable — or merely contingently deferred, keeping the preparation rationale latent?',
    'Track Temple Mount governance, red-heifer breeding programs, priestly-line registry projects, and the political theology of restoration movements; a credibly actionable restoration pathway is the resolving datum.',
    'A live restoration prospect strips the terminal-record axiom of its empirical grounding and pulls the arrangement back toward the preparation reading''s structure; permanent foreclosure hardens the archive frame and lets the dead founding problem stay dead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temple_restoration_contingency, empirical, 'Whether the archive reading''s foundational contingency (non-restoration) holds indefinitely.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the residual suppression keeping students on the sacrificial track structural (funding gates, marriage-market penalties, appointment pipelines) or internalized (formed belief in the corpus''s intrinsic supremacy that travels with the graduate)?',
    'Post-exit trajectory study: follow graduates who left full-time corpus study; if felt obligation and status anxiety persist after every external barrier is removed, the internalized share dominates.',
    'A high internalized share raises effective suppression above the structural measure and strengthens the identity-lock reading of both payer seats; a high structural share predicts rapid decay if funding pluralizes — and explains the falling suppression series as emancipation-driven rather than norm-collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism behind residual curricular conformity.').

omega_variable(
    canonical_closure_vs_prestige_construction,
    'Is the curricular centrality of the hardest, largest corpus section an intrinsic property of closed-canon transmission (any closed canon drives study toward its most difficult third), or a constructed prestige economy serving the academies that certify it?',
    'Cross-community curriculum comparison holding the canon constant: Sephardi, Modern Orthodox, and academic-adjacent programs vary sharply in sacrificial-corpus weight; if weight tracks academy concentration rather than canon logic, construction wins.',
    'If constructed, the diversion component is a removable policy choice and reads as administered rent; if intrinsic, it is the coordination cost of canon maintenance and belongs below the extraction line.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(canonical_closure_vs_prestige_construction, conceptual, 'Whether total-corpus study demand is canon-intrinsic or academy-constructed.').

omega_variable(
    diversion_valuation_disagreement,
    'Are hours diverted from applicable law a harm by the diverted students'' own values, or a loss only under external efficiency standards that the participants reject?',
    'Longitudinal valuation surveys of current students and alumni, plus revealed preference in track choice wherever switching penalties have been lifted.',
    'If participants endorse the trade, the victim declaration overstates the asymmetry and the classification softens toward pure coordination; if participants dissent privately while conforming publicly, the extraction is deeper than the moderate scalar suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversion_valuation_disagreement, preference, 'Whether the diversion harm is participant-endorsed or externally imposed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_archive, 1000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t1000, kodashim_obligation__study_as_archive, theater_ratio, 1000, 0.15).
narrative_ontology:measurement(koda_tr_t1200, kodashim_obligation__study_as_archive, theater_ratio, 1200, 0.19).
narrative_ontology:measurement(koda_tr_t1450, kodashim_obligation__study_as_archive, theater_ratio, 1450, 0.17).
narrative_ontology:measurement(koda_tr_t1700, kodashim_obligation__study_as_archive, theater_ratio, 1700, 0.21).
narrative_ontology:measurement(koda_tr_t1900, kodashim_obligation__study_as_archive, theater_ratio, 1900, 0.24).
narrative_ontology:measurement(koda_tr_t2025, kodashim_obligation__study_as_archive, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(koda_be_t1000, kodashim_obligation__study_as_archive, base_extractiveness, 1000, 0.34).
narrative_ontology:measurement(koda_be_t1200, kodashim_obligation__study_as_archive, base_extractiveness, 1200, 0.41).
narrative_ontology:measurement(koda_be_t1450, kodashim_obligation__study_as_archive, base_extractiveness, 1450, 0.38).
narrative_ontology:measurement(koda_be_t1700, kodashim_obligation__study_as_archive, base_extractiveness, 1700, 0.42).
narrative_ontology:measurement(koda_be_t1900, kodashim_obligation__study_as_archive, base_extractiveness, 1900, 0.46).
narrative_ontology:measurement(koda_be_t2025, kodashim_obligation__study_as_archive, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t1000, kodashim_obligation__study_as_archive, suppression_requirement, 1000, 0.55).
narrative_ontology:measurement(koda_su_t1200, kodashim_obligation__study_as_archive, suppression_requirement, 1200, 0.5).
narrative_ontology:measurement(koda_su_t1450, kodashim_obligation__study_as_archive, suppression_requirement, 1450, 0.42).
narrative_ontology:measurement(koda_su_t1700, kodashim_obligation__study_as_archive, suppression_requirement, 1700, 0.36).
narrative_ontology:measurement(koda_su_t1900, kodashim_obligation__study_as_archive, suppression_requirement, 1900, 0.32).
narrative_ontology:measurement(koda_su_t2025, kodashim_obligation__study_as_archive, suppression_requirement, 2025, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_archive, identity_coordination).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_preparation).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the natural-language label 'the obligation to study Kodashim' covers three structurally distinct claims that share one kernel (kodashim_obligation) and differ on the efficacy-attribution axis. This file authors the archive reading (terminal preservation; victim set = diverted scholarly capacity; beneficiary = communal identity; epsilon 0.45, moderate). The performance sibling authors cosmic-enactment (its study has full functional output, so its victim set empties and its epsilon sits near the coordination floor); the preparation sibling authors messianic stockpiling under continuing legal obligation (added coercive layer raises its suppression and epsilon above this file's). The upstream/downstream citation pattern runs from this file outward in one direction: archive-reading historicization is cited BY critics of the other two readings, while the other two cite the same substitution proof-texts (Menahot 110a) as warrant for their stronger claims. Each member links the other two via affects_constraints; no member averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_obligation__study_as_archive, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
