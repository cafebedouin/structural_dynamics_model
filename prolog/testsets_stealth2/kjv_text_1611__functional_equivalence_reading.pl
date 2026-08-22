% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__functional_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__functional_equivalence_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: kjv_text_1611__functional_equivalence_reading
 *   human_readable: Functional-Equivalence Pluralism of the English Bible
 *   domain: religious_studies/textual_criticism
 *
 * SUMMARY:
 *   This story instantiates one reading of the 1611 King James Bible as a
 *   contested kernel: the functional-equivalence reading, under which no
 *   single English text gates access to scripture. The standing arrangement
 *   it describes is the pluralist settlement that consolidated across
 *   mainline English-speaking Christianity in the second half of the
 *   twentieth century: the 1611 text is retained where its register does work
 *   it alone does, namely public ceremony, memorized psalmody, and literary
 *   continuity, while a rotating population of scholarly modern versions
 *   carries teaching, evangelism, and updated manuscript knowledge. Readers,
 *   congregations, committees, and publishers coordinate by matching texts to
 *   purposes rather than by crowning one mandatory text. Epsilon is authored
 *   for that standing pluralist arrangement as this reading sees it: low,
 *   with residual costs in duplicated translation effort, cross-version
 *   citation friction, and the choice burden the settlement places on
 *   unguided readers. Sibling readings of the same kernel are separate
 *   constraint files linked through the network section; their structures
 *   differ and are not averaged into this one.
 *
 * KEY AGENTS:
 *   - general_bible_readers: primary beneficiary (moderate/mobile) — receives matched texts at trivial switching cost
 *   - modern_translation_committees: agenda-setter and beneficiary (institutional/mobile) — administers the productive side of the settlement and collects standing and revenue
 *   - kjv_liturgical_traditions: beneficiary with identity-attached retention (organized/constrained) — keeps the old register voluntarily
 *   - bible_publishers: commercial beneficiary (organized/arbitrage) — profits from breadth, not from any gatekept text
 *   - textual_criticism_scholars: analytical observer (institutional/analytical) — supplies the findings new versions rest on
 *   - kjv_exclusivist_congregations: excluded dissenting seat (organized/identity_locked) — declines the settlement from outside it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__functional_equivalence_reading, 0.12).
domain_priors:suppression_score(kjv_text_1611__functional_equivalence_reading, 0.08).
domain_priors:theater_ratio(kjv_text_1611__functional_equivalence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__functional_equivalence_reading, rope).
narrative_ontology:human_readable(kjv_text_1611__functional_equivalence_reading, "Functional-Equivalence Pluralism of the English Bible").
narrative_ontology:topic_domain(kjv_text_1611__functional_equivalence_reading, "religious_studies/textual_criticism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__functional_equivalence_reading, '41cc3226-9652-498b-b405-0fc7f1e15d02').
narrative_ontology:cs_kernel_codification('41cc3226-9652-498b-b405-0fc7f1e15d02', fixed_text).
narrative_ontology:cs_authority_grounding('41cc3226-9652-498b-b405-0fc7f1e15d02', distributed).
narrative_ontology:cs_reading_relation('41cc3226-9652-498b-b405-0fc7f1e15d02', kjv_text_1611__exclusive_inspiration_reading, forecloses).
narrative_ontology:cs_reading_relation('41cc3226-9652-498b-b405-0fc7f1e15d02', kjv_text_1611__revisable_translation_reading, coexists_with).
narrative_ontology:cs_axiom('41cc3226-9652-498b-b405-0fc7f1e15d02', foundational, translation_adequacy_is_purpose_relative).
narrative_ontology:cs_axiom_status(translation_adequacy_is_purpose_relative, holdable).
narrative_ontology:cs_axiom_grounding('41cc3226-9652-498b-b405-0fc7f1e15d02', translation_adequacy_is_purpose_relative, instrumental).
narrative_ontology:cs_axiom('41cc3226-9652-498b-b405-0fc7f1e15d02', foundational, no_single_text_holds_gatekeeping_authority).
narrative_ontology:cs_axiom_status(no_single_text_holds_gatekeeping_authority, holdable).
narrative_ontology:cs_axiom_grounding('41cc3226-9652-498b-b405-0fc7f1e15d02', no_single_text_holds_gatekeeping_authority, conventional).
narrative_ontology:cs_reference_frame('41cc3226-9652-498b-b405-0fc7f1e15d02', complementary_purpose_pluralism).
narrative_ontology:cs_drift_state('41cc3226-9652-498b-b405-0fc7f1e15d02', contemporary_digital_access_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('41cc3226-9652-498b-b405-0fc7f1e15d02', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__functional_equivalence_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, general_bible_readers).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, modern_translation_committees).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, kjv_liturgical_traditions).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, bible_publishers).
narrative_ontology:constraint_vindicates(kjv_text_1611__functional_equivalence_reading, purpose_relative_translation_adequacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Read scripture in whichever version suits the occasion: a modern translation for personal study and teaching, the 1611 text for weddings, funerals, and memorized psalms. Editions are inexpensive or free online, switching costs are near zero, and no body tells them which text they must use.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, general_bible_readers, beneficiary,
    moderate, biographical, mobile, global).

% Standing committees of scholars and denominational representatives produce and periodically revise modern versions, publishing under their own names and copyrights. They set the terms on which new translations enter circulation; their reward is scholarly standing, denominational adoption, and book revenue. Nothing stops another committee from producing a rival version.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, modern_translation_committees, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__functional_equivalence_reading, modern_translation_committees, beneficiary).

% Churches and traditions that keep the 1611 text in worship for its cadence, its continuity with centuries of liturgy, and its place in the literary canon. They use modern versions freely for study and outreach while reserving the old register for ceremony. Leaving the old register would cost them recognizable liturgical identity; nothing forces them to leave.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, kjv_liturgical_traditions, beneficiary,
    organized, generational, constrained, global).

% Commercial and nonprofit publishers who print and distribute multiple translations side by side: parallel editions, study Bibles, gift and digital editions. Revenue scales with the variety of texts in circulation rather than with any single mandated version, and they can add or drop a translation as demand shifts.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, bible_publishers, beneficiary,
    organized, biographical, arbitrage, global).

% Academics who compare manuscripts, evaluate translation accuracy, and publish the findings that feed new revisions. They hold no enforcement seat; their influence runs through the committees and seminary curricula that adopt their conclusions.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, textual_criticism_scholars, observer,
    institutional, civilizational, analytical, global).

% Congregations, chiefly in some Baptist and independent circles, that teach the 1611 text is the inspired Word of God in English and decline every other version. They stand outside the pluralist arrangement by conviction, answer its spread with their own schools and presses, and would regard participation in its conversations as compromise. Leaving their position would unravel a communal identity built around the text.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, kjv_exclusivist_congregations, excluded,
    organized, generational, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kjv_text_1611__functional_equivalence_reading, diffuse).
narrative_ontology:fixing_cost_class(kjv_text_1611__functional_equivalence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides the work of scripture access across registers: one early-modern text carries public ceremony, memorization, and literary continuity, while a rotating set of scholarly versions carries accuracy, readability, and updated manuscript knowledge, so communities match texts to uses instead of litigating a single winner.
% TRANSFER_FUNCTION: Moves attention, trust, and modest book revenue among competing texts according to purpose: devotional clarity and teaching flow to modern versions, ceremonial gravitas and quotation flow to the 1611 text; no compulsory transfer runs from any group to a gatekeeper.
% ABSENT_VOICES: Congregations holding the 1611 text exclusively inspired refuse the pluralist table entirely and press their case from separate institutions; lay readers without scholarly guidance also sit uneasily inside the settlement, bearing the choice burden it creates without a seat in the committees that multiply the choices.
% DISAPPEARANCE_RATIONALE: If the functional division vanished overnight, the version conflicts of the mid-twentieth century would reopen: denominations would fight again over which single text is authoritative, seminaries and publishers would realign behind rival standard texts, and readers would inherit whatever battle their tradition won. Multi-version curricula, parallel editions, and ecumenical translation projects all presuppose the settlement.
% FOUNDING_PROBLEM: Recurring destructive conflict over which single English Bible is authoritative: the Revised Version and Revised Standard Version controversies split congregations, while a fixed early-modern text drifted further from living language and from the oldest manuscripts.
% FOUNDING_PROBLEM_CORROBORATION: Historians of British and American religion document the RSV-era controversies and the subsequent drift into functional pluralism, and sociologists studying the KJV-only movement attest both the original conflict and its persistence at the margins. No attestation comes from the exclusivist wing, which denies the founding problem ever existed; that denial is itself the contest recorded here.
narrative_ontology:disappearance_verdict(kjv_text_1611__functional_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__functional_equivalence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__functional_equivalence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kjv_text_1611__functional_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__functional_equivalence_reading, 0.12, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__functional_equivalence_reading_tests).
:- end_tests(kjv_text_1611__functional_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.12): the settlement's residual costs, namely duplicated translation effort, friction when citations cross versions, and publisher margins on a crowded catalog, are real but small, and no seat converts the arrangement into compelled payment. Suppression is near zero (0.08): nothing is forbidden, dissenting congregations run their own presses and schools, and exit in every direction is open. Theater is low (0.15): anniversary celebrations and heritage rhetoric around the 1611 text are performative, but the ceremonial register they decorate does functional work in worship. Accessibility collapse is moderate-low (0.3): for participants the alternative of one mandatory text loses grip once the settlement is understood, but the exclusivist counter-arrangement remains fully livable at the margins, so alternatives do not collapse. Resistance is low (0.2): rejection comes from the exclusivist wing and from periodic fatigue at version proliferation, neither of which threatens the settlement's operation. The temporal series run on one shared grid (points 0, 15, 30, 45, 60, 74 of a 1952-2026 interval) so every tracked metric is authored at every examined point; extraction declines as the settlement consolidates while theater inches up with heritage celebration. Suppression_requirement series are deliberately omitted: enforcement capacity was never the settlement's mechanism, and the static picture is carried by the scalar. Boltzmann coordination type is resource_allocation: the settlement's primary function is allocating text-registers to uses, and its inherent transaction costs sit near the type floor. Receipt surface: gains are authored diffuse after checking every seat, since readers, committees, traditions, and publishers all collect something and none captures a compelled transfer; removal is authored prohibitive because unwinding a working settlement buys nothing and would schism whoever attempted it.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the reader's seat the arrangement is near-pure freedom: matched texts, trivial switching costs. From the committee seat it is a research program with rewards. From the liturgical-tradition seat it is inheritance plus permission: they keep the old register because it is theirs, now without needing to defend it as the only true one. From the exclusivist congregation's seat, which sits outside the arrangement, the same settlement reads as the dissolution of a truth guarantee: what participants experience as pluralism, that seat experiences as abandonment of the text's authority. The publisher seat sees portfolio economics. The engine derives these divergences from power, exit, and role data; this commentary only describes where they come from.
 *
 * DIRECTIONALITY LOGIC:
 *   Every seated party is declared a beneficiary, so derived directionality sits near the subsidy end for all of them: readers get matched texts at trivial cost, committees convert scholarship into standing and sales, traditions keep their register without enforcement burdens, publishers sell breadth. No victim group is declared because the arrangement compels no transfer; the closest candidates, namely unguided readers bearing choice costs and exclusivist congregations losing cultural ground, bear diffuse uncompelled burdens that this reading prices inside epsilon rather than as extraction targets. Global scope raises verification difficulty modestly, but with base extraction this low the amplification moves little. No directionality overrides are used: the beneficiary declarations plus exit options already yield the intended d-values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, destructive conflict over a single authoritative English text, is authored contested: mainline actors treat it as managed, the exclusivist wing denies it ever existed, and historians attest both the original conflicts and their marginal persistence. Because the problem is not dead, no mandatrophy resolution is declared, and the status-times-verdict pair (contested, world_rearranges) raises no zombie flag. The classification guards against mislabeling in both directions: the real coordination function and open alternatives resist reading the settlement as pure extraction, while the low theater ratio and live function resist reading the KJV's ceremonial slot as inertial maintenance, since the heritage ceremony decorates a register that still does liturgical work. The mechanical receipt-surface cell (diffuse gains, prohibitive removal cost) is piton-shaped arithmetic, but the cost-asymmetry test behind it finds no administrator who could change the arrangement and declines to: the settlement has no seat with the power to unwind it, which is the decentralization this reading asserts, not decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the functional-equivalence settlement the actual operating norm of English Bible use, or one reading of the kjv_text_1611 kernel whose rivals would restructure authority if they captured institutions?',
    'Track denominational adoption patterns, seminary curricula, and licensing regimes over time: institutional capture by the exclusive-inspiration reading would re-concentrate gate-keeping in a single text; capture by the revisable reading would subordinate the 1611 text to a revision pipeline.',
    'Under exclusivist capture, extractiveness rises sharply and a payer seat appears covering all non-KJV readers; under revisable capture, the 1611 text''s complementary slot shrinks toward heritage-only. Either outcome dissolves this file''s low-epsilon profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the kernel describes the live arrangement.').

omega_variable(
    coordination_cost_of_decentralization,
    'Do the coordination costs the settlement accepts in exchange for decentralization, namely cross-version citation friction, doctrinal drift between renderings, and choice burden on unguided readers, stay below the extraction a single-gatekeeper regime would impose?',
    'Comparative study of comprehension, cross-reference error, and inter-congregational cooperation under pluralist versus single-text regimes, using the RSV-controversy era as historical control.',
    'If coordination costs exceed avoided extraction, the net-beneficiary claim weakens and the arrangement trends toward a hybrid in which unguided readers effectively subsidize scholarly pluralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cost_of_decentralization, empirical, 'Whether decentralization''s costs undercut its net-benefit structure.').

omega_variable(
    kjv_liturgical_slot_vitality,
    'Is the 1611 text''s retained ceremonial slot a genuinely complementary function, or heritage inertia maintained by performance that a future generation will not renew?',
    'Longitudinal attendance and usage data in traditions that keep the old register, tested against counterfactuals where switching carried no identity cost.',
    'Rising theatricality in the KJV slot would push that niche toward inertial maintenance inside an otherwise healthy settlement, dating a partial lifecycle transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kjv_liturgical_slot_vitality, empirical, 'Vitality of the KJV''s complementary register.').

omega_variable(
    modern_version_copyright_gatekeeping,
    'Does copyright on modern translations reintroduce soft gate-keeping that contradicts the reading''s no-gatekeeper claim, given that the 1611 text is public domain while its rivals are licensed?',
    'Audit licensing terms, quotation permissions, and reuse costs across major modern versions; compare ministry, app-development, and audio-production friction against the public-domain baseline.',
    'Significant licensing friction would raise effective extraction above the authored value and add a payer seat (small ministries, app developers, audio producers) that the current declaration set omits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_version_copyright_gatekeeping, empirical, 'Whether copyright recreates gate-keeping inside the pluralist settlement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__functional_equivalence_reading, 0, 74).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t0, kjv_text_1611__functional_equivalence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(kjv__tr_t0, observed).
narrative_ontology:measurement(kjv__tr_t15, kjv_text_1611__functional_equivalence_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement_basis(kjv__tr_t15, observed).
narrative_ontology:measurement(kjv__tr_t30, kjv_text_1611__functional_equivalence_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement_basis(kjv__tr_t30, observed).
narrative_ontology:measurement(kjv__tr_t45, kjv_text_1611__functional_equivalence_reading, theater_ratio, 45, 0.14).
narrative_ontology:measurement_basis(kjv__tr_t45, observed).
narrative_ontology:measurement(kjv__tr_t60, kjv_text_1611__functional_equivalence_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement_basis(kjv__tr_t60, observed).
narrative_ontology:measurement(kjv__tr_t74, kjv_text_1611__functional_equivalence_reading, theater_ratio, 74, 0.15).
narrative_ontology:measurement_basis(kjv__tr_t74, observed).

% Extraction over time
narrative_ontology:measurement(kjv__be_t0, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(kjv__be_t0, observed).
narrative_ontology:measurement(kjv__be_t15, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement_basis(kjv__be_t15, observed).
narrative_ontology:measurement(kjv__be_t30, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 30, 0.16).
narrative_ontology:measurement_basis(kjv__be_t30, observed).
narrative_ontology:measurement(kjv__be_t45, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 45, 0.14).
narrative_ontology:measurement_basis(kjv__be_t45, observed).
narrative_ontology:measurement(kjv__be_t60, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 60, 0.12).
narrative_ontology:measurement_basis(kjv__be_t60, observed).
narrative_ontology:measurement(kjv__be_t74, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 74, 0.12).
narrative_ontology:measurement_basis(kjv__be_t74, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kjv_text_1611__functional_equivalence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__functional_equivalence_reading, resource_allocation).
narrative_ontology:affects_constraint(kjv_text_1611__functional_equivalence_reading, kjv_text_1611__exclusive_inspiration_reading).
narrative_ontology:affects_constraint(kjv_text_1611__functional_equivalence_reading, kjv_text_1611__revisable_translation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question of the KJV's authority decomposes into three structurally distinct stories over the kjv_text_1611 kernel, each with its own epsilon and beneficiary/victim structure. The exclusive_inspiration_reading authors a high-extraction gatekeeping arrangement (alternatives condemned, exits suppressed); the revisable_translation_reading authors a moderate-extraction improvement regime (the text subordinated to a revision pipeline fed by manuscript scholarship); this file authors the low-extraction pluralist settlement in which the text holds complementary register authority only. The revisable reading is upstream of this one: its manuscript and linguistic findings supply the modern versions whose legitimacy the complementarity claim presupposes. The exclusivist reading contests both and is linked for contamination analysis, not averaging.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
