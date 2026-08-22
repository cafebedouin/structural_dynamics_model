% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__bridge_pidginized
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__bridge_pidginized, []).

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
 *   constraint_id: hebrew_continuity__bridge_pidginized
 *   human_readable: Pan-Diaspora Hebrew Bridge-Language Regime
 *   domain: sociolinguistic/religious-economic
 *
 * SUMMARY:
 *   Between the consolidation of Hebrew printing and the emancipation-era
 *   language shift, Hebrew functioned across the Jewish diaspora as a
 *   supralocal contact language: no community spoke it natively, yet
 *   merchants from Salonika to Amsterdam and Prague to Livorno conducted
 *   correspondence, drew contracts, and adjudicated disputes through it,
 *   while a simplified marketplace register served face-to-face dealings
 *   between Jews of different vernaculars. The arrangement was neither the
 *   liturgy's Hebrew nor a mother tongue: it was maintained by compulsory
 *   communal schooling, sustained by the practical payoff of a single
 *   intercommunal code, and administered by a learned elite whose authority
 *   rested on the same textual mastery the regime demanded of everyone else.
 *   This story is one reading of the hebrew_continuity kernel (see
 *   kernel_context); as a family member it links to its sibling stories,
 *   whose epsilon values differ — the liturgical reading assesses a
 *   low-extraction preservation regime, the native-generative reading
 *   assesses a regime defined by speakerhood, while this story assesses the
 *   bridge arrangement itself: genuinely coordinative, moderately extractive,
 *   actively enforced. KEY AGENTS (by structural relationship): -
 *   long_distance_merchants: Primary coordination beneficiary
 *   (organized/mobile) — collects cross-communal tradability -
 *   rabbinic_scholarly_elite: Agenda-setter and concentrated receipt-seat
 *   (institutional/constrained) — controls curriculum, adjudication, and the
 *   interpretive monopoly - kehillah_councils: Administrative agenda-setter
 *   (institutional/constrained) — funds and mandates schooling -
 *   heder_schoolchildren: Primary acquisition-cost bearer (powerless/trapped)
 *   - women_of_the_kehillot: Excluded cost-bearer (powerless/trapped) —
 *   outside the literate economy the regime maintains -
 *   vernacular_majority_jews: Dual-positioned householder stratum
 *   (moderate/constrained) — pays levies, uses the bridge occasionally -
 *   hebrew_printers_and_scribes: Occupational beneficiary
 *   (moderate/constrained) - maskil_language_reformers: Analytical observer
 *   (moderate/analytical) — late-interval critics
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, 0.48).
domain_priors:suppression_score(hebrew_continuity__bridge_pidginized, 0.4).
domain_priors:theater_ratio(hebrew_continuity__bridge_pidginized, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, extractiveness, 0.48).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__bridge_pidginized, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__bridge_pidginized, "Pan-Diaspora Hebrew Bridge-Language Regime").
narrative_ontology:topic_domain(hebrew_continuity__bridge_pidginized, "sociolinguistic/religious-economic").

domain_priors:requires_active_enforcement(hebrew_continuity__bridge_pidginized).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__bridge_pidginized, '33b18a34-6e5b-42ef-8987-441d5fce8950').
narrative_ontology:cs_kernel_codification('33b18a34-6e5b-42ef-8987-441d5fce8950', fixed_text).
narrative_ontology:cs_authority_grounding('33b18a34-6e5b-42ef-8987-441d5fce8950', lineage).
narrative_ontology:cs_interpretation_layer_present('33b18a34-6e5b-42ef-8987-441d5fce8950').
narrative_ontology:cs_reading_relation('33b18a34-6e5b-42ef-8987-441d5fce8950', hebrew_continuity__liturgical_preservation, coexists_with).
narrative_ontology:cs_reading_relation('33b18a34-6e5b-42ef-8987-441d5fce8950', hebrew_continuity__native_generative, influences).
narrative_ontology:cs_axiom('33b18a34-6e5b-42ef-8987-441d5fce8950', foundational, instrumental_use_constitutes_liveness).
narrative_ontology:cs_axiom_status(instrumental_use_constitutes_liveness, holdable).
narrative_ontology:cs_axiom_grounding('33b18a34-6e5b-42ef-8987-441d5fce8950', instrumental_use_constitutes_liveness, instrumental).
narrative_ontology:cs_axiom('33b18a34-6e5b-42ef-8987-441d5fce8950', foundational, mixed_registers_are_authentic_hebrew).
narrative_ontology:cs_axiom_status(mixed_registers_are_authentic_hebrew, holdable).
narrative_ontology:cs_axiom_grounding('33b18a34-6e5b-42ef-8987-441d5fce8950', mixed_registers_are_authentic_hebrew, conventional).
narrative_ontology:cs_reference_frame('33b18a34-6e5b-42ef-8987-441d5fce8950', supralocal_instrumental_medium).
narrative_ontology:cs_drift_state('33b18a34-6e5b-42ef-8987-441d5fce8950', post_emancipation_language_shift, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('33b18a34-6e5b-42ef-8987-441d5fce8950', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__bridge_pidginized, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, long_distance_merchants).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, rabbinic_scholarly_elite).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, hebrew_printers_and_scribes).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, heder_schoolchildren).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, women_of_the_kehillot).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, vernacular_majority_jews).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, vernacular_majority_jews).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, responsa_network_authority_doctrine).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, hebrew_document_enforceability_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Buy and sell across communal and state boundaries: a trader in Salonika finances goods against the signature of a correspondent in Prague or Amsterdam because Hebrew contracts and letters are recognized everywhere. They pay scribal fees and keep Hebrew correspondence clerks, and they can relocate, change correspondence practice, or shift toward state languages as markets allow — their livelihoods are what made the shared code worth keeping.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, long_distance_merchants, beneficiary,
    organized, biographical, mobile, continental).

% Set the curriculum in the heder and yeshiva, answer responsa sent from across the diaspora, appoint judges and teachers, and decide what counts as correct usage. Their income, standing, and marriage alliances flow through the text economy they administer; stepping outside it would forfeit the authority that constitutes their position.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, rabbinic_scholarly_elite, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__bridge_pidginized, rabbinic_scholarly_elite, beneficiary).

% Lay governing boards drawn from the wealthier householders: levy the taxes that fund schooling and the salaried rabbinate, pass attendance ordinances, license scribes and printers, and discipline lapses through communal bans. They administer the arrangement and also pay into it from their own households.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, kehillah_councils, agenda_setter,
    institutional, generational, constrained, regional).

% Begin Hebrew study at five or six and spend their childhoods decoding prayer, Pentateuch, and Talmudic text in a language nobody around them speaks natively, under teachers paid from their fathers' fees. They cannot leave; the classroom, the curriculum, and its hours are chosen entirely by adults.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, heder_schoolchildren, payer,
    powerless, biographical, trapped, local).

% Run households inside communities whose formal literate economy is closed to them: they pray from Yiddish devotional books, sign business and legal papers by proxy or in vernacular formula, and depend on fathers, husbands, and sons to read or write anything in the shared code. Their sons' schooling consumes household resources they help earn.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, women_of_the_kehillot, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__bridge_pidginized, women_of_the_kehillot, excluded).

% Ordinary householders whose daily life runs in Yiddish, Ladino, or Judeo-Arabic: they pay the education levies and scribal fees, meet Hebrew at prayer and lifecycle events, and need it directly only when dealing beyond their own speech community — where it is the only working option.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, vernacular_majority_jews, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__bridge_pidginized, vernacular_majority_jews, beneficiary).

% Earn their living from the code: composing and proofing Hebrew books for presses serving a continent-wide market, drafting and copying contracts and letters for clients who cannot. Their skills have little value outside the Hebrew book and document trade, so their livelihoods ride on the demand the arrangement generates.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, hebrew_printers_and_scribes, beneficiary,
    moderate, biographical, constrained, continental).

% Late-interval critics of the schooling arrangements: they argue that rote Hebrew acquisition produces neither fluency nor piety, publish grammars and polemics, and propose rebuilding Jewish language life around European languages and reformed Hebrew study. They observe and agitate from outside the administering institutions.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, maskil_language_reformers, observer,
    moderate, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__bridge_pidginized, rabbinic_scholarly_elite).
narrative_ontology:fixing_cost_class(hebrew_continuity__bridge_pidginized, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies one supralocal code for a diaspora whose vernaculars (Yiddish, Ladino, Judeo-Arabic, Judeo-Persian) were mutually unintelligible: standardized Hebrew documents were recognizable and enforceable in any community's court, correspondence crossed communal boundaries without interpreters, and halakhic queries traveled to distant authorities and returned with binding answers.
% TRANSFER_FUNCTION: Moves childhood labor and household resources — tuition, communal education levies, scribal fees — from families, disproportionately from those with no voice in setting the requirement, into maintenance of the shared code; moves commercial enforceability and legal certainty to merchants and courts; moves teaching income, scribal and printing work, and interpretive authority to the learned stratum.
% ABSENT_VOICES: Women of the kehilot had no seat: excluded from Hebrew literacy, they prayed through Yiddish tekhines and transacted through male proxies while bearing the regime's household costs. The schoolchildren subject to the acquisition burden had no voice in its length or method. Poor households paying education levies they could ill afford were heard only through communal charity politics. All stood outside the rabbinic-merchant consensus that set communal language policy; the maskilic critics who eventually voiced some of these grievances arrive only at the interval's end.
% DISAPPEARANCE_RATIONALE: Overnight removal would fracture intercommunal commerce into interpreter-mediated and vernacular-patchwork exchange, void the mutual enforceability of Hebrew instruments, silence the responsa networks through which peripheral communities accessed legal authority, and strand the Hebrew printing trade — pan-diaspora religious and commercial coordination would reorganize around costlier, weaker substitutes within a generation.
% FOUNDING_PROBLEM: After political dispersal, Jewish communities speaking divergent vernaculars still needed to trade, marry, adjudicate, and correspond across communal boundaries; Hebrew, already the inherited sacred and literary code, was repurposed as the shared medium that kept a single dispersed people administrable.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: surviving commercial correspondence archives (the Cairo Geniza and its early-modern successors), communal takkanot mandating schooling and licensing scribes, state and guild records noting the reach of Jewish commercial networks, and modern sociolinguistic scholarship on Jewish intervernacular communication. No attestation rests solely on the rabbinate or the merchant houses that gained from the arrangement.
narrative_ontology:disappearance_verdict(hebrew_continuity__bridge_pidginized, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__bridge_pidginized, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__bridge_pidginized, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_continuity__bridge_pidginized, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__bridge_pidginized, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__bridge_pidginized_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__bridge_pidginized_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claim — tangled_rope — follows from structure visible in the stakeholder surface: a real coordination good (one intercommunal code replacing interpreter patchworks and unenforceable cross-vernacular contracts) delivered through actively enforced arrangements whose costs land asymmetrically (compulsory childhood acquisition, gendered exclusion from literacy, elite interpretive rents). Extractiveness 0.48 prices that asymmetry against the regime's genuine service: substantial rents, not predatory ones. Suppression 0.40 is the raw structural force keeping the arrangement intact — communal education mandates, ban-backed discipline, curriculum control — and is deliberately unscaled; only extractiveness receives engine scaling by directionality and scope. Theater_ratio 0.25: most activity was functional (contracts, responsa, waybills), but ornamental epistolography and pilpul display grew across the interval as mastery-signaling detached from use. Accessibility_collapse 0.50: vernaculars, state languages, and professional interpreters remained real alternatives for many purposes, but nothing else delivered authenticated cross-communal legal instruments, so the alternative set half-collapsed for the regime's core function. Resistance 0.30: episodic complaint about schooling burdens and fees, no organized opposition until the late-interval maskilic critique. boltzmann.coordination_type is information_standard: the load-bearing function is the shared code itself — if the code fails, intercommunal commerce and law fail — while the identity-maintenance overlay rode on vernaculars that persisted independently; declaring identity_coordination would grant the identity framing leeway the FNL gaming warning counsels against. The measurement series runs on one shared grid (t=0..30) for both tracked metrics; suppression_requirement is intentionally not traced because enforcement capacity was broadly static across the interval, a picture already carried by the scalar.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the merchant seat the arrangement is enabling infrastructure — the reason a Salonika consignment can be financed against a Prague signature — and computes toward coordination. From the child and women's seats the same structure is compulsory labor and barred access, computing toward extraction. The elite seat experiences neither: it administers the curriculum, adjudicates by the code, and collects the teaching, scribal, and judicial flows, so the arrangement appears as self-evident order. The engine derives these per-seat classifications from power, exit, and declared position; the divergence between the merchant's coordination-experience and the child's extraction-experience is the perspectival datum, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared positions drive d. long_distance_merchants hold beneficiary position with arbitrage-grade exit (they physically moved between communities and could switch correspondence practice) — nearest the beneficiary pole. rabbinic_scholarly_elite combine agenda-setting with beneficiary position; their exit is constrained because their authority is constituted by the text economy, but they run the arrangement, so d sits low. hebrew_printers_and_scribes collect occupational flows under constrained exit — low-to-mid d. heder_schoolchildren and women_of_the_kehillot are declared victims with trapped exit — nearest the target pole. vernacular_majority_jews are dual-positioned (pay levies and acquisition costs; use the bridge for cross-communal dealings) under constrained exit — near symmetric. kehillah_councils administer and part-pay — slightly below symmetric. No directionality_overrides are authored: the derivation from declared roles, power, and exit reproduces these relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   Mislabel risks run both ways. Reading the regime as pure coordination erases who paid: the acquisition burden fell on children who chose nothing and on women barred from the literacy the regime defined. Reading it as pure extraction erases what it delivered: authenticated cross-communal commerce and law that no vernacular alternative could supply. Tangled_rope holds both facts in one structure. On mandatrophy: the founding problem — dispersed communities needing a shared medium — stayed live throughout the interval, so the arrangement was not yet a zombie; but the rising theater_ratio traces the Goodhart drift (mastery-display detaching from use) that marks where the reading later dissolved: when state integration supplied rival codes, the bridge function emptied and persistence became inertial. The R5 interview records that genealogy; the mismatch consumer should find status=live paired with verdict=world_rearranges here — no zombie flag — with the drift visible only in the measurement slope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_dismissal,
    'This constraint is one reading of the hebrew_continuity kernel, and both sibling readings (liturgical_preservation, native_generative) dismiss bridge usage as ''not really Hebrew'' — is instrumental occupation of the kernel sufficient for continuity, or does liveness require sanctity or nativity?',
    'Cross-reading corpus comparison: classify all three sibling stories and locate the disagreement in the liveness axiom (this reading''s instrumental_use_constitutes_liveness versus the siblings'' sanctity and nativity premises); no in-story data resolves it.',
    'If nativity or sanctity is held necessary, this reading''s constraint dissolves into a utility artifact and its epsilon reassigns to the siblings'' stories; if instrumentality suffices, this reading stands as the operative continuity mechanism for its interval.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_dismissal, conceptual, 'Committer structure: one reading of hebrew_continuity, dismissed by both siblings; the disagreement is located in the liveness axiom.').

omega_variable(
    acquisition_burden_distribution,
    'How heavy was the net acquisition burden on schoolchildren and poor households relative to the returns they or their families ever realized from Hebrew competence?',
    'Archival reconstruction: tuition and communal education ledgers, teacher-contract records, memoir evidence (Glickel of Hameln, Solomon Maimon), and probate inventories weighing books against tools.',
    'A heavier net burden on non-consenting payers pushes effective extraction upward and the computed type toward snare; a lighter burden supports the coordination-heavy reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acquisition_burden_distribution, empirical, 'Distribution of the regime''s acquisition costs across consenting and non-consenting payers.').

omega_variable(
    gendered_exclusion_offset,
    'How complete was women''s exclusion from the bridge''s benefits, and did informal channels (Yiddish tekhines, household business participation, female philanthropy) offset the barred access?',
    'Probate and ketubah records, surviving women''s letters, tekhine publication patterns, and communal charity rolls showing women''s economic roles.',
    'Deeper uncompensated exclusion raises the victim-weighted extraction measure and strengthens the payer declaration for women_of_the_kehillot; substantial offsets pull their directionality back toward symmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gendered_exclusion_offset, empirical, 'Depth of gendered exclusion from the literate economy the regime maintained.').

omega_variable(
    pidgin_register_autonomy,
    'Was the marketplace pidgin a stable autonomous register — a genuine third form of Hebrew life — or merely defective performance of the high register?',
    'Contact-linguistic analysis of surviving commercial formulae, waybills, and phrase-lists against high-register epistolary norms.',
    'An autonomous register confirms this reading describes a distinct constraint with its own epsilon; a merely defective register collapses this story toward the liturgical_preservation description and merges their metric profiles.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pidgin_register_autonomy, conceptual, 'Whether the spoken bridge register was a real third form or derivative performance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__bridge_pidginized, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__bridge_pidginized, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t6, hebrew_continuity__bridge_pidginized, theater_ratio, 6, 0.17).
narrative_ontology:measurement_basis(hebr_tr_t6, observed).
narrative_ontology:measurement(hebr_tr_t12, hebrew_continuity__bridge_pidginized, theater_ratio, 12, 0.19).
narrative_ontology:measurement_basis(hebr_tr_t12, observed).
narrative_ontology:measurement(hebr_tr_t18, hebrew_continuity__bridge_pidginized, theater_ratio, 18, 0.21).
narrative_ontology:measurement_basis(hebr_tr_t18, observed).
narrative_ontology:measurement(hebr_tr_t24, hebrew_continuity__bridge_pidginized, theater_ratio, 24, 0.23).
narrative_ontology:measurement_basis(hebr_tr_t24, observed).
narrative_ontology:measurement(hebr_tr_t30, hebrew_continuity__bridge_pidginized, theater_ratio, 30, 0.25).
narrative_ontology:measurement_basis(hebr_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__bridge_pidginized, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t6, hebrew_continuity__bridge_pidginized, base_extractiveness, 6, 0.43).
narrative_ontology:measurement_basis(hebr_be_t6, observed).
narrative_ontology:measurement(hebr_be_t12, hebrew_continuity__bridge_pidginized, base_extractiveness, 12, 0.45).
narrative_ontology:measurement_basis(hebr_be_t12, observed).
narrative_ontology:measurement(hebr_be_t18, hebrew_continuity__bridge_pidginized, base_extractiveness, 18, 0.46).
narrative_ontology:measurement_basis(hebr_be_t18, observed).
narrative_ontology:measurement(hebr_be_t24, hebrew_continuity__bridge_pidginized, base_extractiveness, 24, 0.47).
narrative_ontology:measurement_basis(hebr_be_t24, observed).
narrative_ontology:measurement(hebr_be_t30, hebrew_continuity__bridge_pidginized, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(hebr_be_t30, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_continuity__bridge_pidginized, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__bridge_pidginized, information_standard).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__native_generative).

% DUAL FORMULATION NOTE:
% Constraint family: hebrew_continuity decomposes into three readings with distinct epsilon values. This story authors epsilon at 0.48 for the bridge arrangement itself (genuine coordination, asymmetric costs, active enforcement). The liturgical_preservation sibling authors a low-extraction preservation regime; the native_generative sibling authors a regime defined by native speakerhood. Each is a separate file with its own beneficiaries, victims, and metrics; the colloquial label 'Hebrew stayed alive' conflates them. Edges here record family membership, not causal dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
