% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__user_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__user_centric_reading, []).

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
 *   constraint_id: fair_use_four_factor_test__user_centric_reading
 *   human_readable: Fair Use as Affirmative User Right (User-Centric Reading)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This story instantiates the user-centric reading of the fair-use
 *   four-factor kernel: fair use as an affirmative right belonging to the
 *   public, weighed to protect access, education, criticism, and cultural
 *   production, with market-harm-to-the-creator treated as one factor among
 *   four rather than a controlling presumption. Under this reading,
 *   unauthorized use by educators, libraries, critics, and remix creators is
 *   low-extraction (ε ~0.28) because the doctrine's own internal logic treats
 *   such use as the intended and legitimate operation of the system, not a
 *   deviation from it. This is distinct from the creator-centric reading
 *   (which frames the same conduct as an exception carved narrowly out of a
 *   strong property default, and would author much higher ε from the
 *   rights-holder vantage) and the transformative-use reading (which turns on
 *   transformativeness as the dominant axis regardless of user-class,
 *   potentially licensing highly transformative commercial reuse that this
 *   reading would treat more cautiously). Per the ε-invariance principle,
 *   each reading is authored here as its own constraint with its own stable ε
 *   — this file speaks only for the user-centric reading.
 *
 * KEY AGENTS:
 *   - educators_and_students: Primary beneficiary (moderate/constrained) — obtains access without licensing cost
 *   - libraries_and_archives: Primary beneficiary (organized/constrained) — preserves and provides access to cultural heritage
 *   - documentarians_and_critics: Beneficiary (moderate/constrained) — incorporates material into public-interest commentary
 *   - remix_and_commentary_creators: Beneficiary (powerless/constrained) — most exit-constrained beneficiary class
 *   - individual_rights_holders: Primary target (moderate/constrained) — bears uncompensated use of their work
 *   - content_licensing_intermediaries: Secondary target (organized/constrained) — loses brokered transaction volume
 *   - major_media_conglomerates: Excluded institutional voice (institutional/mobile) — structurally subordinated in this reading's framing
 *   - courts: Agenda-setter (institutional/analytical) — administers the four-factor balancing test
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__user_centric_reading, 0.28).
domain_priors:suppression_score(fair_use_four_factor_test__user_centric_reading, 0.22).
domain_priors:theater_ratio(fair_use_four_factor_test__user_centric_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__user_centric_reading, rope).
narrative_ontology:human_readable(fair_use_four_factor_test__user_centric_reading, "Fair Use as Affirmative User Right (User-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__user_centric_reading, "legal/intellectual_property/cultural_production").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__user_centric_reading, '9e16aefa-ac15-44c9-8df9-fed59db6b152').
narrative_ontology:cs_kernel_codification('9e16aefa-ac15-44c9-8df9-fed59db6b152', formalized).
narrative_ontology:cs_authority_grounding('9e16aefa-ac15-44c9-8df9-fed59db6b152', practice).
narrative_ontology:cs_interpretation_layer_present('9e16aefa-ac15-44c9-8df9-fed59db6b152').
narrative_ontology:cs_reading_relation('9e16aefa-ac15-44c9-8df9-fed59db6b152', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('9e16aefa-ac15-44c9-8df9-fed59db6b152', fair_use_four_factor_test__transformative_use_reading, influences).
narrative_ontology:cs_axiom('9e16aefa-ac15-44c9-8df9-fed59db6b152', foundational, fair_use_is_affirmative_public_right).
narrative_ontology:cs_axiom_status(fair_use_is_affirmative_public_right, holdable).
narrative_ontology:cs_axiom_grounding('9e16aefa-ac15-44c9-8df9-fed59db6b152', fair_use_is_affirmative_public_right, conventional).
narrative_ontology:cs_axiom('9e16aefa-ac15-44c9-8df9-fed59db6b152', secondary, market_harm_is_one_factor_not_controlling).
narrative_ontology:cs_axiom_status(market_harm_is_one_factor_not_controlling, holdable).
narrative_ontology:cs_axiom_grounding('9e16aefa-ac15-44c9-8df9-fed59db6b152', market_harm_is_one_factor_not_controlling, instrumental).
narrative_ontology:cs_reference_frame('9e16aefa-ac15-44c9-8df9-fed59db6b152', public_interest_purpose_of_copyright).
narrative_ontology:cs_drift_state('9e16aefa-ac15-44c9-8df9-fed59db6b152', contemporary_digital_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9e16aefa-ac15-44c9-8df9-fed59db6b152', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, educators_and_students).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, libraries_and_archives).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, documentarians_and_critics).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, remix_and_commentary_creators).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, researchers).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, individual_rights_holders).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, content_licensing_intermediaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, major_media_conglomerates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use excerpts, images, and clips in classrooms, coursepacks, and research without seeking permission, relying on fair use to make teaching and scholarship affordable and timely. Without the doctrine read this way, they would face licensing costs and delays that would curtail what they can teach or study.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, educators_and_students, beneficiary,
    moderate, biographical, constrained, national).

% Digitize, preserve, and provide access to collections — including orphan works and out-of-print materials — under the user-right reading, which treats preservation and access as core to fair use's purpose rather than incidental to it. Licensing-based alternatives would leave large portions of cultural heritage inaccessible or lost.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, libraries_and_archives, beneficiary,
    organized, generational, constrained, national).

% Quote, excerpt, and incorporate copyrighted material into criticism, commentary, and nonfiction filmmaking. The user-centric reading weighs their public-interest function heavily, allowing use even where a rights holder would prefer to license or refuse.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, documentarians_and_critics, beneficiary,
    moderate, biographical, constrained, global).

% Produce derivative online content — parody, mashups, reaction and analysis videos — that incorporates copyrighted source material. They have no bargaining position to negotiate licenses individually; the user-right reading is often the only thing standing between their work and takedown.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, remix_and_commentary_creators, beneficiary,
    powerless, immediate, constrained, global).

% Text-and-data-mine large corpora, quote source material in publications, and build on prior work under the assumption that fair use protects the public-knowledge function of research, not merely narrow quotation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, researchers, beneficiary,
    moderate, generational, constrained, global).

% Independent authors, photographers, musicians, and small studios whose work is used without payment or permission under the user-centric weighing. They bear lost licensing revenue and diminished control over how and where their work circulates, with litigation as their only recourse — expensive and uncertain against fair-use defenses courts read broadly in the user's favor.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, individual_rights_holders, payer,
    moderate, biographical, constrained, national).

% Collective licensing bodies, stock agencies, and rights clearance services whose business models depend on mandatory licensing transactions. A user-centric fair use doctrine shrinks the transactions they can broker, since more uses fall outside the licensing requirement entirely.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, content_licensing_intermediaries, payer,
    organized, biographical, constrained, national).

% Hold large copyrighted catalogs and would prefer a narrower, creator-centric fair use doctrine that maximizes licensing leverage. Their institutional voice is present in litigation and lobbying but is structurally subordinated in the user-centric reading's own framing, which treats their preferences as a countervailing interest to be balanced down, not a starting presumption.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, major_media_conglomerates, excluded,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__user_centric_reading, major_media_conglomerates, payer).

% Apply the four statutory factors case by case, and under this reading orient the balancing test around whether the use serves public access, education, criticism, or cultural production — treating market harm to the rights holder as one factor among four rather than the dominant consideration.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__user_centric_reading, diffuse).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__user_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows society to use, teach, criticize, preserve, and build upon existing creative and informational works without a licensing transaction for every use, solving the holdup and transaction-cost problem that would otherwise choke off criticism, education, and preservation.
% TRANSFER_FUNCTION: Moves the economic value of otherwise-licensable uses from rights holders (who forgo licensing revenue) to public-facing and downstream users (who receive the use without payment), justified by the public benefit the use generates.
% ABSENT_VOICES: Individual rights holders whose works are used non-commercially or transformatively rarely appear in the cases that set doctrine — the landmark fair-use opinions are disproportionately fought by well-resourced defendants (studios, tech platforms, universities) against plaintiffs who often cannot afford to litigate, meaning the user-right reading is shaped by whichever user had standing and money, not by the median individual creator's interest.
% DISAPPEARANCE_RATIONALE: If courts stopped reading fair use as an affirmative user right and instead treated it as a narrow, disfavored exception, licensing markets would expand sharply: educators would need clearance for coursepacks, documentarians would need clearance for archival footage, and remix culture online would face far more takedowns. Cultural production would shift toward licensed, pre-cleared content, and public-interest institutions (libraries, universities) would need much larger budgets or would restrict access.
% FOUNDING_PROBLEM: Copyright grants creators exclusive rights, but rigid exclusivity would prevent criticism, scholarship, news reporting, and preservation that the public needs and that copyright's own constitutional purpose (promoting progress) depends on — someone must be able to quote, excerpt, and build without asking permission every time.
% FOUNDING_PROBLEM_CORROBORATION: Library associations, educational institutions, and public-interest legal scholars outside the user community itself (e.g., amicus filings from historians and preservation groups in landmark cases) attest that the access and preservation problem remains unsolved by licensing markets alone — orphan works and out-of-print materials in particular have no functioning licensing channel, corroborating the problem's persistence independent of the beneficiaries' own advocacy.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__user_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__user_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__user_centric_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_four_factor_test__user_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__user_centric_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__user_centric_reading_tests).
:- end_tests(fair_use_four_factor_test__user_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28) because, from this reading's own structural logic, the beneficiary classes are not extracting rents — they are exercising a right the doctrine defines as theirs. The cost that lands on individual rights holders is real but is treated internally as the acceptable price of the public benefit, not as an extraction the system should suppress. Suppression is low-moderate (0.22): the doctrine does not coerce rights holders into compliance so much as withhold a cause of action from them once a use is classified as fair; there is no active enforcement apparatus compelling participation, only litigation risk allocation. Theater ratio is low (0.20) because the four-factor analysis is a substantively operative judicial process, not a performative gloss — courts genuinely weigh the factors, even if this reading tilts the weighing. Accessibility collapse is moderate (0.30): rights holders retain litigation as a channel to contest specific uses, so alternatives to accepting the user's use are not fully foreclosed, merely expensive and uncertain. Resistance is moderately high (0.55) reflecting active, well-funded pushback from rights holders and licensing intermediaries who litigate and lobby against this reading's expansiveness.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (educators, libraries, remix creators), this reading of fair use looks like rope: a genuine, functioning coordination mechanism that lets cultural and educational activity proceed without transaction-cost paralysis. From the individual-rights-holder seat, the same structure looks considerably more extractive: uncompensated use of their labor, justified by a public-interest rationale they did not consent to and cannot easily contest. The engine computes these divergent seat classifications from the same structural data; the claimed type (rope) reflects the reading's own internal self-justification, authored independently of whether the metrics would validate it from every seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (educators, libraries, documentarians, remix creators, researchers) sit near the full-beneficiary end of directionality: the constraint, under this reading, subsidizes their activity by removing a licensing requirement they would otherwise bear. Individual rights holders and licensing intermediaries sit near the target end: the same doctrine, applied the same way, removes compensation and negotiating leverage they would otherwise have. Major media conglomerates are excluded from the reading's own beneficiary framing despite institutional power and mobile exit — their preferred narrower doctrine is the creator-centric sibling reading, not this one; their exclusion here is a structural feature of which reading is instantiated, not an oversight.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rigid exclusivity would strangle criticism, scholarship, and preservation) remains live by the corroboration of library and educational institutions outside the immediate beneficiary community. This weighs against mandatrophy: the user-centric reading is not a vestigial justification for an arrangement that has outlived its purpose — the access and preservation problem it solves is undiminished, arguably intensified by digital-era licensing friction and the scale of orphan-works backlogs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    user_right_vs_narrow_exception_framing,
    'Is fair use structurally an affirmative right held by users (as this reading holds), or a narrow exception carved out of a presumptively strong exclusive property right (the creator-centric sibling reading)? The four-factor statutory text itself does not resolve which default the balancing starts from.',
    'Doctrinal history and comparative analysis of how courts have framed the burden of proof and the presumption direction across circuits and eras; a shift in which party bears the burden of persuasion at factor one would be direct evidence of which reading a given court panel is operating under.',
    'If the user-right framing does not reflect the actual operative default in most circuits, this reading''s low ε (0.28) would be descriptively inaccurate for the jurisdictions where the narrow-exception default in fact governs, and the creator-centric reading would be the structurally dominant one in practice even where user-right rhetoric is used.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_right_vs_narrow_exception_framing, conceptual, 'Whether fair use''s structural default is user-right or narrow-exception is itself contested and framing-dependent.').

omega_variable(
    beneficiary_class_composition_and_power_capture,
    'Does the user-centric reading, in practice, primarily benefit genuinely resource-poor public-interest actors (individual educators, small libraries, independent critics), or has it been substantially captured by well-resourced institutional users (large tech platforms, major universities, streaming aggregators) who invoke the public-interest rationale while extracting commercial value?',
    'Empirical survey of reported fair-use decisions by defendant type and commercial scale over the interval; a rising share of institutional/commercial defendants relative to individual public-interest defendants would indicate capture.',
    'If institutional capture is substantial, the true beneficiary set diverges from the declared beneficiaries (educators, libraries, individual critics) and part of the measured low ε is misattributed — some of what looks like public-interest subsidy is actually a subsidy to well-resourced commercial users, which would push the constraint toward tangled_rope rather than rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_class_composition_and_power_capture, empirical, 'Whether declared public-interest beneficiaries are the actual primary beneficiaries or a cover for institutional capture.').

omega_variable(
    individual_rights_holder_litigation_asymmetry,
    'Given that individual rights holders rarely have the resources to litigate a fair-use dispute to judgment, is the low measured resistance from that seat a genuine indication of low harm, or an artifact of suppressed access to the courts that would otherwise register resistance?',
    'Compare settlement/cease-and-desist rates (which do not appear in reported case law) against litigated outcomes; a large gap would indicate substantial unlitigated resistance invisible to doctrinal analysis.',
    'If most rights-holder resistance is absorbed pre-litigation (informal demand letters, unenforced grievances, simple acquiescence due to cost), the authored resistance value (0.55) understates the true friction this reading generates, and the doctrine''s low-extraction self-image is partly an artifact of who can afford to contest it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_rights_holder_litigation_asymmetry, empirical, 'Whether low visible resistance from individual rights holders reflects genuine acceptance or litigation-cost-driven suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__user_centric_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_four_factor_test__user_centric_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fair_tr_t8, fair_use_four_factor_test__user_centric_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(fair_tr_t16, fair_use_four_factor_test__user_centric_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(fair_tr_t24, fair_use_four_factor_test__user_centric_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(fair_tr_t32, fair_use_four_factor_test__user_centric_reading, theater_ratio, 32, 0.19).
narrative_ontology:measurement(fair_tr_t40, fair_use_four_factor_test__user_centric_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(fair_be_t8, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(fair_be_t16, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 16, 0.22).
narrative_ontology:measurement(fair_be_t24, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 24, 0.25).
narrative_ontology:measurement(fair_be_t32, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(fair_be_t40, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 40, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(fair_use_four_factor_test__user_centric_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__user_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__user_centric_reading, 0.1).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__transformative_use_reading).

% DUAL FORMULATION NOTE:
% Constraint family: fair_use_four_factor_test decomposes into three sibling readings sharing one statutory kernel (the four factors) but instantiating structurally distinct constraints with different ε, different beneficiary/victim sets, and different classifications. creator_centric_reading treats fair use as a narrow exception to a strong property default (high ε from the user's conduct, rights holders as primary beneficiary). transformative_use_reading makes transformativeness the dominant axis, subordinating market harm whenever new meaning is added, regardless of user class. user_centric_reading (this file) treats fair use as an affirmative public right weighed toward access and cultural production, with low ε on the beneficiary side and rights holders as the victim class. Each reading is authored as an independent ε-invariant constraint per the decomposition principle; they are linked here rather than merged because measuring the same statutory text through different framings yields materially different ε values — the classic ε-invariance decomposition signal.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
