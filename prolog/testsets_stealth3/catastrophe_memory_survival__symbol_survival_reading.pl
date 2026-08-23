% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__symbol_survival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__symbol_survival_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__symbol_survival_reading
 *   human_readable: Practice-Continuity Survival Regime (Symbol-Survival Reading)
 *   domain: religious/collective-memory/ritual-practice
 *
 * SUMMARY:
 *   POST-CATASTROPHE PRACTICE-CONTINUITY REGIME. After catastrophic rupture
 *   (the destruction of 70 CE in the classical case; the Holocaust in the
 *   modern case that anchors this story's interval), Jewish communal
 *   authorities institutionalized an answer to dissolution: the people
 *   survives BY practicing — unbroken continuity of ritual form is itself
 *   survival, and those who cease practicing are counted among the dead
 *   lineage even while living. Rabbinic authority adjudicates correct form
 *   (conversion, marriage, divorce, burial, recognition), converting the
 *   survival imperative into a gatekeeping apparatus. This file instantiates
 *   the SYMBOL-SURVIVAL READING of the kernel catastrophe_memory_survival:
 *   the claim that ritual preserves identity and boundary-norms through
 *   symbolic experience, and that survival is continuity of practice itself.
 *   Per the epsilon-referent rule, the referent of extractiveness is the
 *   standing arrangement under contest — the institutionalized
 *   practice-continuity regime with its enforcement machinery — assessed by
 *   this reading's own lights. The reading affirms the constitutive thesis
 *   (practice-continuity is what survival consists in) AND finds the
 *   arrangement built atop that thesis substantially extractive: the thesis
 *   is operationalized into who-counts rules whose costs fall on secularized
 *   Jews, intermarried descendants, and women, while interpretive control
 *   concentrates in the rabbinic class. The sibling readings
 *   (competence_transmission_reading, hybrid_encoding_reading) are separate
 *   constraints in separate files, linked via network.affects_constraints;
 *   their structural deltas differ and are documented in the
 *   kernel_reading_commitment omega. KEY AGENTS (by structural relationship):
 *   see key_agents.
 *
 * KEY AGENTS:
 *   - rabbinic_interpretive_authority: Primary beneficiary and agenda-setter (institutional/identity_locked) — defines correct practice, adjudicates who counts, collects interpretive control; exit would dissolve the self
 *   - israeli_chief_rabbinate: Secondary agenda-setter (institutional/arbitrage) — state-backed recognition monopoly extending the regime's enforcement into civil status
 *   - observant_practicing_lay_communities: Net beneficiary with heavy compliance costs (organized/constrained) — receives identity, meaning, mutual aid; pays tuition, practice labor, endogamy pressure
 *   - secularized_diaspora_jews: Primary target (moderate/constrained) — physically mobile but categorically bound; narrated as transmission failures without consent
 *   - patrilineal_and_intermarried_descendants: Sharpest target edge (moderate/constrained) — counted out by descent rules regardless of practice or identification
 *   - women_in_traditional_communities: Dual-positioned target (moderate/constrained) — bear domestic ritual labor (kashrut, Shabbat preparation, transmission to children) while excluded from public ritual authority
 *   - kiruv_continuity_outreach_sector: Incidental beneficiary (organized/mobile) — funded and justified by the persistence of the continuity crisis it addresses
 *   - non_orthodox_denominational_leadership: Excluded contestant (organized/mobile) — their conversions, marriages, and ordination are objects of the rules, never co-authored
 *   - jewish_studies_historians: Analytical observer (analytical/analytical) — document the transmission debate from outside the benefiting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, 0.76).
domain_priors:suppression_score(catastrophe_memory_survival__symbol_survival_reading, 0.74).
domain_priors:theater_ratio(catastrophe_memory_survival__symbol_survival_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__symbol_survival_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__symbol_survival_reading, "Practice-Continuity Survival Regime (Symbol-Survival Reading)").
narrative_ontology:topic_domain(catastrophe_memory_survival__symbol_survival_reading, "religious/collective-memory/ritual-practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__symbol_survival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__symbol_survival_reading, '3e0de5ad-36ce-4d4f-998c-571b38fd692d').
narrative_ontology:cs_kernel_codification('3e0de5ad-36ce-4d4f-998c-571b38fd692d', formalized).
narrative_ontology:cs_authority_grounding('3e0de5ad-36ce-4d4f-998c-571b38fd692d', lineage).
narrative_ontology:cs_interpretation_layer_present('3e0de5ad-36ce-4d4f-998c-571b38fd692d').
narrative_ontology:cs_reading_relation('3e0de5ad-36ce-4d4f-998c-571b38fd692d', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e0de5ad-36ce-4d4f-998c-571b38fd692d', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('3e0de5ad-36ce-4d4f-998c-571b38fd692d', foundational, practice_continuity_constitutes_survival).
narrative_ontology:cs_axiom_status(practice_continuity_constitutes_survival, holdable).
narrative_ontology:cs_axiom_grounding('3e0de5ad-36ce-4d4f-998c-571b38fd692d', practice_continuity_constitutes_survival, deontological).
narrative_ontology:cs_axiom('3e0de5ad-36ce-4d4f-998c-571b38fd692d', secondary, form_fidelity_over_adaptive_revision).
narrative_ontology:cs_axiom_status(form_fidelity_over_adaptive_revision, holdable).
narrative_ontology:cs_axiom_grounding('3e0de5ad-36ce-4d4f-998c-571b38fd692d', form_fidelity_over_adaptive_revision, conventional).
narrative_ontology:cs_reference_frame('3e0de5ad-36ce-4d4f-998c-571b38fd692d', unbroken_practice_transmission).
narrative_ontology:cs_drift_state('3e0de5ad-36ce-4d4f-998c-571b38fd692d', contemporary_secularization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3e0de5ad-36ce-4d4f-998c-571b38fd692d', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, rabbinic_interpretive_authority).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, observant_practicing_lay_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, kiruv_continuity_outreach_sector).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, secularized_diaspora_jews).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, patrilineal_and_intermarried_descendants).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, women_in_traditional_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, women_in_traditional_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines correct ritual form through codes, responsa, and communal adjudication; controls the gates (conversion, marriage, divorce, burial, recognition) that operationalize who counts as continuing the lineage. Collects interpretive authority, institutional position, and adjudication power directly from the regime. Exit is not realistically available: a decisor's authority, training, social world, and self-concept are fused with the system — leaving would dissolve the self, not just the career.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, rabbinic_interpretive_authority, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__symbol_survival_reading, rabbinic_interpretive_authority, beneficiary).

% Holds a state-backed monopoly over marriage, divorce, burial, and conversion recognition in Israel, and extends recognition decisions to diaspora converts and diaspora rabbinates. Its arbitration position is protected by state law rather than communal consent alone, so pressure from diaspora denominations or secularized publics does not translate into exit pressure on it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, israeli_chief_rabbinate, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive identity, meaning structure, mutual-aid networks, and assurance of continuity from the practice regime. Simultaneously bear its heaviest direct compliance costs: day-school tuition, practice labor, endogamy pressure on their children, and the discipline of form-fidelity. Leaving would cost them community, family standing, and the identity they live by, so exit is priced far above its monetary cost.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, observant_practicing_lay_communities, beneficiary,
    organized, generational, constrained, global).

% Have ceased halakhic practice while retaining Jewish identification. The regime counts them as transmission failures — losses to the lineage — a categorization they did not consent to and cannot appeal. They pay in categorical terms (exclusion from peoplehood claims, continuity-crisis rhetoric aimed at them) and in kinship terms (family pressure, holiday-table friction), though they pay no compliance labor and sit outside the active enforcement perimeter. Physically mobile; categorically bound.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secularized_diaspora_jews, payer,
    moderate, biographical, constrained, continental).

% Children of intermarriage or patrilineal descent who may practice, identify, and contribute, but whom descent rules count out of the lineage regardless. They bear the sharpest edge of the form-standard: no amount of practice purchases recognition under matrilineal descent, and seeking recognition exposes them to conversion gatekeeping of years-long scrutiny.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, patrilineal_and_intermarried_descendants, payer,
    moderate, biographical, constrained, national).

% Carry much of the regime's daily transmission labor — kashrut, Shabbat preparation, children's early ritual education — while being excluded from public ritual authority and decisorship. They receive identity and community belonging alongside the labor burden, and divorce-related gatekeeping (agunot) binds some of them to marriages the regime itself adjudicates. Exit costs family and community standing.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, women_in_traditional_communities, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__symbol_survival_reading, women_in_traditional_communities, beneficiary).

% Outreach organizations, campus programs, and heritage initiatives funded to reverse attrition. Their revenue, staffing, and institutional purpose depend on the continuity crisis persisting; a world in which secularized Jews were simply legitimate variants of peoplehood would eliminate their mandate. They recruit from the victim pool and are rewarded for the problem's continuation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, kiruv_continuity_outreach_sector, beneficiary,
    organized, biographical, mobile, global).

% Clergy and institutions of Reform, Conservative, Reconstructionist, and Renewal streams. Their ordinations, conversions, and marriages are objects of the regime's validity rules — routinely rejected or downgraded by the adjudicating authorities — while they hold no seat in the adjudication itself. They contest the monopoly publicly and build parallel institutions, but the recognition gates remain closed to them.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, non_orthodox_denominational_leadership, excluded,
    organized, generational, mobile, continental).

% Academic scholars documenting what ritual has transmitted across catastrophes, testing the symbol-survival claim against the competence-transmission and hybrid accounts, and measuring whether peoplehood persisted through practice or through other carriers. They take no side in the enforcement dispute and collect nothing from the regime's operation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, jewish_studies_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a recognizable, portable peoplehood across dispersion, hostility, and assimilation pressure: shared calendar, shared texts, mutual-recognition rituals, and mutual-aid obligations — membership criteria solved once, centrally, rather than renegotiated by each scattered community.
% TRANSFER_FUNCTION: Moves interpretive authority and life-cycle adjudication power (marriage, divorce, conversion, burial, recognition) from lay members to the rabbinic class; moves compliance labor (practice performance, tuition, endogamy pressure, domestic transmission work) from individuals and families into the system; moves status — 'continuer of the lineage' — to those who conform to the form-standard, and the counter-status of 'loss' to those who do not.
% ABSENT_VOICES: Non-Orthodox denominational leadership is structurally outside the adjudication table — their validity judgments are overridden by authorities they cannot sit with. Secularized Jews themselves are absent as speakers: the regime narrates them as losses without their consent, and the descendants already gone have no seat from which to contest the counting. Both would object that the survival definition is being enforced on people who were never asked to accept it.
% DISAPPEARANCE_RATIONALE: If the practice-continuity standard and its enforcement vanished overnight, the recognition gates (conversion, marriage, burial, who-is-a-Jew) would lose their object; rabbinic interpretive authority would lose its monopoly justification; the excluded denominations and secularized Jews would be re-described as legitimate variants rather than losses; and the outreach sector's mandate would evaporate. Marriage markets, school systems, and diaspora-Israel recognition arrangements would all reorganize around whatever membership criteria replaced form-continuity.
% FOUNDING_PROBLEM: After catastrophic rupture — the destruction of 70 CE in the classical case, the Holocaust in the case anchoring this interval — a decimated and dispersed people faced dissolution: under what conditions does it remain a people? The institutionalized answer: it survives by practicing; unbroken continuity of ritual form is itself the survival, and lapses are deaths in the chain.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox adjudicating authorities attest the founding problem is live, citing assimilation and intermarriage as slow-motion continuation of the catastrophe. Outside the benefiting parties, academic demography (large-scale Pew-type surveys of Jewish identification), historians of the Haskalah and emancipation, and Israeli sociologists of religion attest that the acute post-catastrophe dissolution threat passed with the reconstruction decades, that peoplehood demonstrably persists through non-practice carriers (language, statehood, ethnic identification), and that the regime's rising enforcement now partly serves institutional interest. No single corroborating source speaks for all parties; the contest itself is the attested finding.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__symbol_survival_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__symbol_survival_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__symbol_survival_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_survival__symbol_survival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__symbol_survival_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   CLAIM/METRIC INDEPENDENCE: claimed_type is tangled_rope because the structure possesses BOTH a genuine coordination function AND asymmetric extraction under active enforcement. The coordination function is real and historically load-bearing: a dispersed, periodically persecuted population maintained mutual recognition, a shared calendar, mutual-aid obligations, and portable membership criteria across continents for two millennia — solved once, centrally, rather than per-community. The extraction is equally real: the same apparatus converts the survival imperative into gatekeeping whose gains concentrate (interpretive authority, adjudication power over marriage/divorce/burial, institutional revenue) while costs diffuse (compliance labor, tuition burdens, endogamy pressure, categorical exclusion of the non-practicing, agunot). Hence requires_active_enforcement: true, named beneficiaries, named victims. METRICS: extractiveness 0.76 (high, per this reading's structural delta — the form-preservation demand extracts heavily from those who cannot or will not maintain form); suppression 0.74 (structural gatekeeping plus state-backed monopoly in Israel plus internalized continuity-guilt; see the suppression_mechanism_internalization omega for the structural/internalized split); theater_ratio 0.35 (core practice remains substantively performed — this is not a piton — but a growing share of activity is performative maintenance: public piety signaling, continuity galas, heritage performance detached from transmission); accessibility_collapse 0.58 (alternatives exist and are exercised — Reform, secular, Humanistic Judaism — but within the traditionalist episteme, accepting the survival-equals-practice premise collapses alternatives into 'death of the lineage,' raising perceived exit costs sharply); resistance 0.62 (two centuries of sustained resistance: Haskalah, Reform, secular Zionism, intermarriage defection, feminist ritual challenge — the liberal movements were historically precisely the victim-coalition this constraint's structure invites). BOLTZMANN GAMING ALERT: coordination_type is identity_coordination, and this constraint is a paradigm case of the flagged risk — identity framing ('this is who we are') doubling as extraction cover. The complexity offset accommodates genuine boundary-maintenance work, but the Power-by-Scope coupling here concentrates gains on an institutional seat at global scope while costs land on moderate-power agents at continental scope; the coupling is nonsensical-as-coordination to the extent the symbolic register functions as cover. TEMPORAL DYNAMICS: the series run on one shared nine-point grid. Extraction and suppression rise monotonically with a cyclical rider — each assimilation scare (1960s attrition panic, 1990 intermarriage report, post-2023 solidarity surge) triggers an outreach-and-enforcement ratchet that does not fully relax; the oscillation is itself partly an extraction mechanism (crisis fundraising and recruitment ride intermittent reinforcement), with the baseline ratchet visible in the series.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inside the same nominal religion compute different constraints. The agenda-setter seat (rabbinic_interpretive_authority) experiences the arrangement as sacred trust and survival infrastructure — from inside, enforcement IS fidelity. The payer-inside seats (observant laity, women) experience costly-but-meaningful participation: real goods received, real labor extracted, exit priced in family and community terms. The payer-outside seat (secularized Jews) experiences a narration it never consented to — being counted as a loss by an institution it left. The excluded seat (non-Orthodox clergy) experiences an illegitimate monopoly: their validity judgments are overridden by a table they cannot sit at. The engine computes these divergences from the structural data (power, exit, role); the divergence between the agenda-setter's rope-experience and the outside-payer's snare-experience is the measurement this corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive derivation: rabbinic_interpretive_authority (agenda_setter + beneficiary, identity_locked) derives near the full-beneficiary end — the regime subsidizes its authority directly. israeli_chief_rabbinate similarly near-beneficiary with arbitrage-grade exit (state backing insulates it from diaspora opinion). kiruv_continuity_outreach_sector derives low-d as beneficiary, accurately: it collects funding and purpose from the crisis's persistence. TWO OVERRIDES CORRECT DERIVATION FAILURES: (1) observant_practicing_lay_communities — role-derived d would sit near 0.1-0.15 (pure beneficiary), but this seat bears the regime's largest direct compliance costs (day-school tuition, practice labor, endogamy constraint); its true position is near-symmetric, overridden to 0.38. (2) secularized_diaspora_jews — victim-role plus constrained-exit would derive d near full-target, but this seat sits OUTSIDE the active collection perimeter: it pays no compliance labor, and its extraction is categorical (being counted as loss) plus residual kinship pressure; overridden to 0.72. The victim_self_selection_boundary omega documents the uncertainty this override embeds. Patrilineal descendants and women derive high-d from their victim declarations without override — both sit inside or at the gate of the enforcement perimeter.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing dissolution of a catastrophically decimated, dispersed people — had an acute phase that the reconstruction decades substantially resolved; the arrangement nonetheless persists with RISING enforcement (suppression series climbs throughout the interval), which is the classic mandate-outlived-function signature. Unlike a piton, however, gains remain concentrated (rabbinic_interpretive_authority is a named capture seat), so the regime is actively maintained by someone who profits — inertia alone does not explain it. The tangled_rope classification prevents two mislabels: calling it rope ignores the named victims and the gatekeeping extraction; calling it snare erases the genuine coordination achievement (mutual recognition and mutual aid that demonstrably preserved peoplehood through hostile centuries — the coordination function is not cover). The R5 mismatch consumer is pointedly relevant here: founding_problem_status is authored 'contested' (Orthodox authorities attest the dissolution threat is live and accelerating; demographers and historians outside the benefiting parties attest the acute phase passed and persistence now partly serves institutional interest). If the status resolves to dead while disappearance_verdict stays world_rearranges, the zombie flag fires — and given rising enforcement against a receding acute threat, that flag would be correct.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (symbol_survival_reading) of the kernel catastrophe_memory_survival; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative compilation of the three sibling stories: competence_transmission_reading relocates the value of ritual to embedded practical knowledge (making discontinuity a skills-loss, not an identity-loss, and shifting the victim set toward communities that abandoned adaptive protocols); hybrid_encoding_reading splits epsilon across a symbolic register and a practical register, diluting the concentration of extraction documented here. The disagreement is located in WHAT ritual transmits — symbolic identity versus practical competence versus both — which determines who counts as harmed by discontinuity and who collects from enforcing continuity.',
    'If the competence reading is adopted as primary, the victim set shifts from secularized Jews (categorized as identity-losses) to whole communities losing adaptive knowledge, and rabbinic interpretive control loses its monopoly justification; if the hybrid reading is adopted, this story''s epsilon partitions and the tangled_rope verdict may soften toward rope on the symbolic register alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: this file instantiates one reading of a contested kernel; sibling readings are separate constraints with different epsilon, victim sets, and classifications.').

omega_variable(
    constructed_vs_discovered_survival_doctrine,
    'Is survival-through-practice-continuity a discovered structural truth about group persistence under dispersion and hostility, or a constructed doctrine whose enforcement happens to concentrate interpretive authority in the class that preaches it?',
    'Comparative historical study of non-practice-based peoplehood persistence: secular Yiddish cultural continuity, Armenian and Roma diaspora identity maintenance, and cases where communities survived catastrophe without form-enforcement. If comparable persistence occurs without practice-gatekeeping, the doctrine is constructed and the FSM question activates.',
    'If constructed, the constraint''s naturality claim collapses and the measured extraction reclassifies toward pure rent-collection riding a genuine coordination substrate; if discovered, part of the measured extraction is the irreducible price of the coordination itself and the rope component is larger than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_discovered_survival_doctrine, empirical, 'Whether the practice-continuity doctrine is a natural law of group survival or a constructed constraint with identifiable beneficiaries.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression structural (gatekeeping over marriage, conversion, burial, recognition; agunot; state-backed monopoly) or internalized (the continuity-guilt and deficiency narrative that secularized Jews carry after exiting practice)?',
    'Post-exit suppression trajectory: interview secularized Jews and intermarried descendants who left the observant frame decades ago; if the deficiency narrative and family-pressure costs persist undiminished after all structural gates were exited, a substantial share of suppression is internalized.',
    'If internalized, effective suppression exceeds the structural measure — targets carry the constraint with them after exit, raising effective extraction for the diaspora victim seats and strengthening the case that exit is categorically blocked even when physically open.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural versus internalized suppression mechanism in the continuity regime.').

omega_variable(
    victim_self_selection_boundary,
    'Are secularized diaspora Jews genuinely victims of this constraint, or self-selected defectors who reject the constraint''s categories and therefore bear little imposed cost?',
    'Survey and ethnographic measurement of whether secularized Jews experience the continuity discourse as coercive (guilt induction, family sanction, categorical exclusion from peoplehood claims) or as irrelevant noise from an institution they ignore.',
    'If largely irrelevant to them, effective extraction concentrates on agents still inside or knocking at the gate (intermarried descendants seeking recognition, converts, agunot), shrinking the victim set and shifting the constraint''s extraction profile toward gate-crushing rather than diffuse identity-taxation; the directionality override for this seat would then move further toward the beneficiary end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_self_selection_boundary, empirical, 'Whether the declared primary victim set experiences the constraint as extraction or has already exited its jurisdiction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__symbol_survival_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 60, 0.31).
narrative_ontology:measurement(cata_tr_t70, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 70, 0.33).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 80, 0.35).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 60, 0.73).
narrative_ontology:measurement(cata_be_t70, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 70, 0.75).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 80, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 50, 0.64).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(cata_su_t70, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 70, 0.71).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 80, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__symbol_survival_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'ritual preserves survival through catastrophe memory' decomposes into three structurally distinct claims per the epsilon-invariance principle. This file carries the symbol-survival claim (identity and boundary-norms via symbolic experience; survival = practice continuity) with high epsilon, rabbinic capture, and secularized-Jew victim set. The competence-transmission sibling carries the practical-knowledge claim with a different victim set (communities losing adaptive protocols) and different beneficiaries; the hybrid sibling partitions epsilon across both registers. Upstream/downstream: this reading upstream-influences the hybrid (its symbolic register descends from this account); the competence reading stands as a coexisting rival account. Each member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_survival__symbol_survival_reading, organized, 0.38).
constraint_indexing:directionality_override(catastrophe_memory_survival__symbol_survival_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
