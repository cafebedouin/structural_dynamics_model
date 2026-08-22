% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__literary_revival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__literary_revival_reading, []).

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
 *   constraint_id: hebrew_living_language__literary_revival_reading
 *   human_readable: Haskalah Written Generative Chain (Literary Revival Reading)
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   Between roughly 1780 and 1900, a dispersed elite of Maskilic writers,
 *   editors, and readers maintained Hebrew as a generatively written language
 *   — novels, journalism, popular science, and polemic composed anew in a
 *   tongue nobody spoke daily. This story instantiates ONE reading of the
 *   contested kernel hebrew_living_language: that this unbroken written chain
 *   constitutes the language's continuing life. The sibling readings —
 *   liturgical_continuity_reading (recitation and study suffice) and
 *   native_generation_reading (only native daily speech counts) — are
 *   separate constraints with their own epsilon values and victim structures;
 *   per the epsilon-invariance principle they are linked through the network,
 *   not merged here. The claim/metric gap is deliberate: the reading CLAIMS
 *   rope (a voluntary, net-beneficial coordination of a republic of letters),
 *   while the authored metrics describe very low but nonzero extraction
 *   concentrated at the editor seat; the engine computes per-seat
 *   classifications from the structural data and the divergence is the
 *   measurement taken.
 *
 * KEY AGENTS:
 *   - haskalah_maskilic_writers: primary producers (moderate/mobile) — supply generative written Hebrew; bear chronic underpayment
 *   - hebrew_press_editors: administering seat (organized/constrained) — run the periodicals, set norms, retain the thin-market surplus
 *   - hebrew_reading_public: subscribing beneficiaries (moderate/mobile) — sustain demand; carry opportunity costs against the vernacular press
 *   - enlightenment_patrons: financing seat (powerful/mobile) — subsidize recurring losses; collect honor and editorial influence
 *   - traditionalist_rabbinic_leadership: excluded opposition (institutional/trapped) — object to secular use of the sacred tongue; no standing in the maskilic sphere
 *   - historians_of_language_revival: analytical observer (analytical/analytical) — retrospective assessment of the chain and its inheritance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__literary_revival_reading, 0.1).
domain_priors:suppression_score(hebrew_living_language__literary_revival_reading, 0.12).
domain_priors:theater_ratio(hebrew_living_language__literary_revival_reading, 0.16).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, theater_ratio, 0.16).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__literary_revival_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__literary_revival_reading, "Haskalah Written Generative Chain (Literary Revival Reading)").
narrative_ontology:topic_domain(hebrew_living_language__literary_revival_reading, "historical_linguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__literary_revival_reading, 'ba3b466a-9083-4c36-94cf-e2d354a54ca8').
narrative_ontology:cs_kernel_codification('ba3b466a-9083-4c36-94cf-e2d354a54ca8', distributed).
narrative_ontology:cs_authority_grounding('ba3b466a-9083-4c36-94cf-e2d354a54ca8', diffuse_epistemic).
narrative_ontology:cs_reading_relation('ba3b466a-9083-4c36-94cf-e2d354a54ca8', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('ba3b466a-9083-4c36-94cf-e2d354a54ca8', hebrew_living_language__native_generation_reading, forecloses).
narrative_ontology:cs_axiom('ba3b466a-9083-4c36-94cf-e2d354a54ca8', foundational, written_generative_competence_suffices_for_vitality).
narrative_ontology:cs_axiom_status(written_generative_competence_suffices_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('ba3b466a-9083-4c36-94cf-e2d354a54ca8', written_generative_competence_suffices_for_vitality, empirically_contingent).
narrative_ontology:cs_axiom('ba3b466a-9083-4c36-94cf-e2d354a54ca8', foundational, productive_composition_differs_from_memorized_recitation).
narrative_ontology:cs_axiom_status(productive_composition_differs_from_memorized_recitation, holdable).
narrative_ontology:cs_axiom_grounding('ba3b466a-9083-4c36-94cf-e2d354a54ca8', productive_composition_differs_from_memorized_recitation, conventional).
narrative_ontology:cs_reference_frame('ba3b466a-9083-4c36-94cf-e2d354a54ca8', unbroken_written_transmission_chain).
narrative_ontology:cs_drift_state('ba3b466a-9083-4c36-94cf-e2d354a54ca8', post_native_revival_retrospective, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('ba3b466a-9083-4c36-94cf-e2d354a54ca8', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__literary_revival_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, haskalah_maskilic_writers).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, hebrew_reading_public).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, hebrew_press_editors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, enlightenment_patrons).
narrative_ontology:constraint_victim(hebrew_living_language__literary_revival_reading, enlightenment_patrons).
narrative_ontology:constraint_vindicates(hebrew_living_language__literary_revival_reading, written_generative_competence_sufficiency).
narrative_ontology:constraint_vindicates(hebrew_living_language__literary_revival_reading, pan_diaspora_intellectual_unity_via_hebrew).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compose poetry, fiction, essays, scientific popularization, and correspondence in Hebrew for periodicals such as Ha-Me'assef, Ha-Melitz, and Ha-Shahar, usually alongside day jobs in commerce, teaching, or the professions. Payment is irregular and frequently below subsistence; standing within maskil circles is the more reliable return. Most command German, Yiddish, Russian, or Polish and could publish in those languages for larger audiences and steadier pay, so remaining in Hebrew trades income for position in the renewal project.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, haskalah_maskilic_writers, beneficiary,
    moderate, biographical, mobile, continental).

% Found and operate the periodicals and publishing houses that constitute the Hebrew public sphere: select manuscripts, set orthographic and stylistic norms, decide which genres appear, and mediate between contributors, patrons, and subscribers. They collect subscription revenue and patronage, and they set the terms on which contributors are paid — terms that the historical record shows were persistently unfavorable to the people who supplied the copy.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, hebrew_press_editors, agenda_setter,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__literary_revival_reading, hebrew_press_editors, beneficiary).

% A few thousand to tens of thousands of literate Jews across Eastern and Central Europe who subscribe to Hebrew periodicals, read Maskilic prose and verse, and write letters to editors. They also read Yiddish and European-language papers; choosing the Hebrew press costs more effort and money than the alternatives and functions socially as an act of allegiance to the renewal project.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, hebrew_reading_public, beneficiary,
    moderate, biographical, mobile, continental).

% Wealthy individuals and community boards who subsidize printing costs, endow prizes, and absorb the recurring losses of Hebrew journals that never reach commercial viability. In return they receive dedications, honorifics, and informal influence over editorial direction.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, enlightenment_patrons, payer,
    powerful, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__literary_revival_reading, enlightenment_patrons, beneficiary).

% Lead yeshivot and communal religious institutions and regard the deployment of the sacred tongue for secular belles-lettres as a category violation; some place bans on maskilic books, others simply refuse engagement. Their objections carry decisive weight inside the traditional sphere but find no standing in the maskilic press, which prints satire at their expense.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, traditionalist_rabbinic_leadership, excluded,
    institutional, generational, trapped, continental).

% Retrospective scholarly seat: reconstruct publication runs, circulation figures, and correspondence networks; assess whether written generative competence kept the language alive and what the later spoken revival inherited from this layer. Holds no position inside the arrangement itself.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, historians_of_language_revival, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__literary_revival_reading, hebrew_press_editors).
narrative_ontology:fixing_cost_class(hebrew_living_language__literary_revival_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single supralocal high register in which geographically dispersed communities sharing no spoken vernacular can publish, argue, correspond, and transmit a continuously updated written culture; it solves the collective-action problem of maintaining shared norms of grammar, coinage, and style with no state, academy, or enforcement apparatus behind them.
% TRANSFER_FUNCTION: Moves compositional labor and attention from writers to the reading public; moves subscription fees and patronage money from readers and wealthy backers to editors and printers; and moves prestige toward published Hebrew authors, with editors retaining whatever surplus separates what the thin market pays from what the work costs to produce.
% ABSENT_VOICES: Traditionalist rabbinic authorities objecting to secular use of the sacred tongue sit outside the maskilic public sphere entirely; Yiddish and European-language Jewish writers, whose outlets reached far larger audiences, are treated as the road not taken rather than as participants; and women, overwhelmingly excluded from the text-centered education that conferred access, are absent from both the producer and subscriber rolls.
% DISAPPEARANCE_RATIONALE: The periodicals, correspondence networks, prize endowments, and literary careers organized around Hebrew composition would dissolve overnight; the coinages, neologisms, and stylistic norms accumulated across the century — the substrate later speakers drew on — would not exist in inheritable form, and the pan-diaspora intellectual conversation the journals carried would fragment back into separate vernacular spheres.
% FOUNDING_PROBLEM: Diaspora Jewry spoke mutually unintelligible vernaculars — Yiddish, Ladino, Judeo-Arabic, Polish, German — while sharing only a liturgical-and-commentarial Hebrew; the Haskalah set out to renew Hebrew as a living medium of pan-Jewish intellectual exchange and a bridge into European letters.
% FOUNDING_PROBLEM_CORROBORATION: Maskilic manifestos attest the problem self-interestedly, so corroboration is taken from outside the beneficiary set: traditionalist opponents conceded the fragmentation of Jewish communal discourse while disputing the remedy, and the near-simultaneous rise of Yiddish and German-language Jewish presses addressing the identical need shows the problem was real independently of the Hebrew solution. Whether the problem remains live after the spoken revival is disputed between the sibling readings.
narrative_ontology:disappearance_verdict(hebrew_living_language__literary_revival_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__literary_revival_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__literary_revival_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_living_language__literary_revival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__literary_revival_reading, 0.1, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__literary_revival_reading_tests).
:- end_tests(hebrew_living_language__literary_revival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very low (0.10): participation is voluntary, costs are opportunity costs and chronic underpayment rather than coerced transfer, and the benefit — a shared renewed medium — accrues broadly to participants. Suppression is low (0.12): nothing forbids writing in German or Yiddish (Mendelssohn himself chiefly did), and the chain's persistence reflects preference and conviction, not barriers; the residual value registers the social price of defection inside maskil circles. Theater_ratio (0.16) is low and declining across the interval: early Ha-Me'assef-era biblical pastiche was substantially display, while the later press — news, criticism, science popularization — became functionally communicative. Accessibility_collapse (0.25) is low: understanding the arrangement collapses no alternative, since vernacular and European-language routes stayed open throughout. Resistance (0.20) is modest: friction came from traditionalist bans and satirical counterattack rather than from participants. The temporal series share one grid (t=0..120, step 20) so every tracked metric is authored at every examined point; theater declines as function displaces display, extraction creeps up with professionalization and editor retention of surplus, then plateaus. No suppression_requirement series is authored: the arrangement had no enforcement machinery to build up or decay, so the static scalar captures the whole picture.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the editor seat the arrangement is a fragile republic of letters they personally keep solvent — administration, not capture. From the writer seat the same structure operates as a prestige-for-subsistence trade whose terms are set unilaterally by the people who owe them money. From the reader seat it is a priced act of allegiance. From the excluded rabbinic seat it is a desecration proceeding without license. The engine computes these per-seat classifications from power, exit, and directional position; the authored rope claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   All three core participant groups are declared beneficiaries with mobile or constrained exits, driving their derived d toward the subsidized end; effective extraction is damped accordingly, and the continental scope's amplification acts on a very small base epsilon. Enlightenment_patrons sit near symmetric — they pay real money and collect real honor and influence. Traditionalist_rabbinic_leadership appears in neither the beneficiary nor victim arrays: they stand outside the arrangement's flows, and their exclusion is registered as absence from the conversation, not as extraction borne. The only quasi-transfer with a settled destination is contributor labor flowing to editor balance sheets, recorded on the receipt surface.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both directions. Against mislabeling as snare: the transfers are voluntary, alternatives were open, and no victim class exists — reading underpayment as pure predation would erase the genuine coordination achieved. Against mislabeling as rope-forever: the receipt surface names a capturing seat (editors) with a prohibitive fix cost rooted in thin-market economics, which is exactly the seed from which coordination decays toward extraction if the market thickens or editorial control consolidates. The R5 interview marks the founding problem contested rather than dead, so no zombie flag fires: the arrangement retired with its era rather than persisting past its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the kernel hebrew_living_language; which sibling reading is adopted changes the structural facts — what exactly shifts?',
    'Adjudicate the vitality criterion directly: comparative sociolinguistic analysis of languages sustained by written elite production versus recitation traditions versus native-acquisition speech communities.',
    'Under liturgical_continuity_reading the beneficiary set widens to all synagogue-attached Jews and the generativity requirement drops out; under native_generation_reading this arrangement fails the aliveness test outright and the story''s subject becomes preparation for life rather than life itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: reading choice determines the beneficiary set and the pass/fail structure of the whole story.').

omega_variable(
    identity_lock_vs_market_exit,
    'Did writers persist in Hebrew composition through conviction (identity fused with the renewal project) or through thin-market calculation that happened to price prestige attractively?',
    'Biographical panel: track language-switch decisions of writers facing concrete offers — the documented defections to German and Yiddish journalism — against subsequent career and reputational outcomes.',
    'If identity-locked, the chain''s persistence exceeds what market logic predicts and exit-based measures understate it; if market-driven, the chain was always one better offer away from collapse and its stability is thinner than the rope framing suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_market_exit, empirical, 'Source of writer persistence: identity fusion versus economic calculus.').

omega_variable(
    strict_reachability_ambiguity,
    'Does the written chain constitute reachability of the language for the community at large, or only for the educated male elite who controlled its production?',
    'Circulation and literacy data: estimate the fraction of diaspora Jewry with working access to Maskilic Hebrew at interval midpoint and end, against total literate population.',
    'Strict reachability confines the arrangement''s real coverage to a sliver, concentrating effective per-capita significance and supporting the elite-practice framing; loose reachability extends beneficiary standing toward the broader reading public and strengthens the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_reachability_ambiguity, conceptual, 'Whether aliveness-through-writing reaches beyond the literate elite.').

omega_variable(
    editor_surplus_extraction_or_necessity,
    'Is the persistent contributor-to-editor surplus — the chronic underpayment of Hebrew writers — discretionary capture, or the unavoidable economics of a market too thin to pay fair rates?',
    'Ledger reconstruction: compare journal revenues, patronage inflows, and editor compensation against contributor payments across the major periodicals over the interval.',
    'If capture, the editor seat carries mild tangled_rope pressure and the receipt surface hardens; if necessity, epsilon sits at the coordination-cost floor and the rope claim stands essentially unqualified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(editor_surplus_extraction_or_necessity, empirical, 'Nature of the editor-retained surplus in the Hebrew press economy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__literary_revival_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(heb_lit_revival_tr_t0, hebrew_living_language__literary_revival_reading, theater_ratio, 0, 0.24).
narrative_ontology:measurement_basis(heb_lit_revival_tr_t0, observed).
narrative_ontology:measurement(heb_lit_revival_tr_t20, hebrew_living_language__literary_revival_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(heb_lit_revival_tr_t20, observed).
narrative_ontology:measurement(heb_lit_revival_tr_t40, hebrew_living_language__literary_revival_reading, theater_ratio, 40, 0.21).
narrative_ontology:measurement_basis(heb_lit_revival_tr_t40, observed).
narrative_ontology:measurement(heb_lit_revival_tr_t60, hebrew_living_language__literary_revival_reading, theater_ratio, 60, 0.19).
narrative_ontology:measurement_basis(heb_lit_revival_tr_t60, observed).
narrative_ontology:measurement(heb_lit_revival_tr_t80, hebrew_living_language__literary_revival_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement_basis(heb_lit_revival_tr_t80, observed).
narrative_ontology:measurement(heb_lit_revival_tr_t100, hebrew_living_language__literary_revival_reading, theater_ratio, 100, 0.17).
narrative_ontology:measurement_basis(heb_lit_revival_tr_t100, observed).
narrative_ontology:measurement(heb_lit_revival_tr_t120, hebrew_living_language__literary_revival_reading, theater_ratio, 120, 0.16).
narrative_ontology:measurement_basis(heb_lit_revival_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(heb_lit_revival_be_t0, hebrew_living_language__literary_revival_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(heb_lit_revival_be_t0, observed).
narrative_ontology:measurement(heb_lit_revival_be_t20, hebrew_living_language__literary_revival_reading, base_extractiveness, 20, 0.07).
narrative_ontology:measurement_basis(heb_lit_revival_be_t20, observed).
narrative_ontology:measurement(heb_lit_revival_be_t40, hebrew_living_language__literary_revival_reading, base_extractiveness, 40, 0.08).
narrative_ontology:measurement_basis(heb_lit_revival_be_t40, observed).
narrative_ontology:measurement(heb_lit_revival_be_t60, hebrew_living_language__literary_revival_reading, base_extractiveness, 60, 0.09).
narrative_ontology:measurement_basis(heb_lit_revival_be_t60, observed).
narrative_ontology:measurement(heb_lit_revival_be_t80, hebrew_living_language__literary_revival_reading, base_extractiveness, 80, 0.1).
narrative_ontology:measurement_basis(heb_lit_revival_be_t80, observed).
narrative_ontology:measurement(heb_lit_revival_be_t100, hebrew_living_language__literary_revival_reading, base_extractiveness, 100, 0.11).
narrative_ontology:measurement_basis(heb_lit_revival_be_t100, observed).
narrative_ontology:measurement(heb_lit_revival_be_t120, hebrew_living_language__literary_revival_reading, base_extractiveness, 120, 0.1).
narrative_ontology:measurement_basis(heb_lit_revival_be_t120, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_living_language__literary_revival_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__literary_revival_reading, information_standard).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__native_generation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Hebrew remained a living language' decomposes into three structurally distinct claims per the epsilon-invariance principle. This file (literary_revival_reading) authors epsilon near 0.10 for an elite voluntary literary economy with no victim set and partial continuity via the unbroken written chain. liturgical_continuity_reading authors a near-zero-epsilon mass practice with a vastly wider beneficiary set. native_generation_reading treats the same centuries as non-life and locates the constraint's birth in the 1880s-1920s speech revival, with a correspondingly different failure mode. Each story carries its own beneficiaries, victims, and type; the family is linked so contamination and inheritance analyses can traverse it. Upstream/downstream: the written chain (this reading) supplied the lexical and stylistic substrate that the native-generation outcome consumed, which is why this story influences — without foreclosing — the empirical terrain its sharpest sibling disputes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
