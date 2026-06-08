% ============================================================================
% CONSTRAINT STORY: transmission_fidelity_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-06-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transmission_fidelity_mechanism, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: transmission_fidelity_mechanism
 *   human_readable: Halakhic Transmission Fidelity Mechanism for Non-Performable Temple Sacrifice Law
 *   domain: religious_law/halakhic_authority/commitment_system
 *
 * SUMMARY:
 *   The halakhic transmission mechanism for Temple sacrifice law represents a
 *   unique commitment system phenomenon: an institutional apparatus
 *   maintaining extraordinary fidelity to procedural details for a practice
 *   that has had zero enactment pathway for 1,900 years. Following the
 *   destruction of the Second Temple in 70 CE, the physical performance of
 *   animal sacrifice became structurally impossible (no Temple, no priesthood
 *   in ritual purity, no altar). Yet the rabbinic tradition not only
 *   preserved the legal corpus governing sacrifice but continued to elaborate
 *   it, adjudicate novel questions within it, and structure yeshiva
 *   curriculum around its study. The Talmudic tractate Zevachim (animal
 *   offerings) and Menachot (meal offerings) contain detailed discussions of
 *   sacrifice procedures, ritual purity requirements, and edge cases that
 *   could not be tested or performed. This constraint exhibits the piton
 *   signature: high theater ratio (detailed study and adjudication serving
 *   institutional maintenance rather than functional preparation), low
 *   extraction (the mechanism coordinates genuine scholarly activity and
 *   preserves cultural knowledge), and mandatrophy (the original function —
 *   training priests for Temple service — has atrophied, but the transmission
 *   apparatus persists because it vindicates rabbinic authority and
 *   structures pedagogy). The constraint is a reading of a contested kernel:
 *   different Jewish denominations and theological frameworks interpret the
 *   sacrifice obligation differently, producing structurally distinct
 *   constraints with different beneficiary/victim sets and extractiveness
 *   profiles.
 *
 * KEY AGENTS:
 *   - Rabbinic Interpretive Authority: Primary beneficiary (institutional/arbitrage) — the transmission mechanism vindicates the unbroken-chain doctrine and the authority to redefine mitzvah modality (study as fulfillment)
 *   - Yeshiva Institutional Structure: Primary beneficiary (institutional/constrained) — the transmission mechanism structures curriculum, maintains institutional continuity, and provides pedagogical content
 *   - Observant Student: Identity-locked participant (powerless/identity_locked) — experiences the study obligation as immutable divine command; exit would require abandoning the halakhic framework entirely
 *   - Messianic Restoration Coalition: Organized agents (organized/mobile) — view the transmission mechanism as instrumental preparation with a sunset (Temple rebuilding)
 *   - Reform Jewish Observer: External observer (moderate/mobile) — has exited the binding-obligation frame; sees the transmission mechanism as vestigial ritual
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes the constraint as a commitment system whose primary function has atrophied while institutional apparatus persists
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transmission_fidelity_mechanism, 0.15).
domain_priors:suppression_score(transmission_fidelity_mechanism, 0.2).
domain_priors:theater_ratio(transmission_fidelity_mechanism, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transmission_fidelity_mechanism, extractiveness, 0.15).
narrative_ontology:constraint_metric(transmission_fidelity_mechanism, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(transmission_fidelity_mechanism, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transmission_fidelity_mechanism, piton).
narrative_ontology:human_readable(transmission_fidelity_mechanism, "Halakhic Transmission Fidelity Mechanism for Non-Performable Temple Sacrifice Law").
narrative_ontology:topic_domain(transmission_fidelity_mechanism, "religious_law/halakhic_authority/commitment_system").

domain_priors:requires_active_enforcement(transmission_fidelity_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transmission_fidelity_mechanism, 'f95537e5-fae8-4023-bbfa-0e9a9a310bd9').
narrative_ontology:cs_kernel_codification('f95537e5-fae8-4023-bbfa-0e9a9a310bd9', fixed_text).
narrative_ontology:cs_authority_grounding('f95537e5-fae8-4023-bbfa-0e9a9a310bd9', lineage).
narrative_ontology:cs_interpretation_layer_present('f95537e5-fae8-4023-bbfa-0e9a9a310bd9').
narrative_ontology:cs_reading_relation('f95537e5-fae8-4023-bbfa-0e9a9a310bd9', transmission_fidelity_mechanism__performance_only_reading, forecloses).
narrative_ontology:cs_reading_relation('f95537e5-fae8-4023-bbfa-0e9a9a310bd9', transmission_fidelity_mechanism__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('f95537e5-fae8-4023-bbfa-0e9a9a310bd9', transmission_fidelity_mechanism__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('f95537e5-fae8-4023-bbfa-0e9a9a310bd9', foundational, study_constitutes_performance).
narrative_ontology:cs_axiom_status(study_constitutes_performance, holdable).
narrative_ontology:cs_axiom_grounding('f95537e5-fae8-4023-bbfa-0e9a9a310bd9', study_constitutes_performance, deontological).
narrative_ontology:cs_axiom('f95537e5-fae8-4023-bbfa-0e9a9a310bd9', foundational, rabbinic_authority_to_redefine_modality).
narrative_ontology:cs_axiom_status(rabbinic_authority_to_redefine_modality, holdable).
narrative_ontology:cs_axiom_grounding('f95537e5-fae8-4023-bbfa-0e9a9a310bd9', rabbinic_authority_to_redefine_modality, conventional).
narrative_ontology:cs_axiom('f95537e5-fae8-4023-bbfa-0e9a9a310bd9', secondary, unbroken_transmission_from_sinai).
narrative_ontology:cs_axiom_status(unbroken_transmission_from_sinai, holdable).
narrative_ontology:cs_axiom_grounding('f95537e5-fae8-4023-bbfa-0e9a9a310bd9', unbroken_transmission_from_sinai, theological).
narrative_ontology:cs_reference_frame('f95537e5-fae8-4023-bbfa-0e9a9a310bd9', temple_era_priesthood_authority).
narrative_ontology:cs_drift_state('f95537e5-fae8-4023-bbfa-0e9a9a310bd9', contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f95537e5-fae8-4023-bbfa-0e9a9a310bd9', '2025-06-07T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transmission_fidelity_mechanism, rabbinic_interpretive_authority).
narrative_ontology:constraint_beneficiary(transmission_fidelity_mechanism, yeshiva_institutional_structure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(transmission_fidelity_mechanism, messianic_restoration_coalition).
narrative_ontology:constraint_victim(transmission_fidelity_mechanism, observant_student).
narrative_ontology:constraint_vindicates(transmission_fidelity_mechanism, unbroken_transmission_doctrine).
narrative_ontology:constraint_vindicates(transmission_fidelity_mechanism, study_as_mitzvah_fulfillment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the halakhic framework and adjudicates what counts as mitzvah fulfillment. Benefits from the transmission mechanism as it vindicates the unbroken-chain doctrine (authority derives from continuity with Temple-era priesthood) and the power to redefine performance modality (study = fulfillment). Could reframe sacrifice law as purely historical (as Reform Judaism has done) but maintains binding-obligation frame because it serves institutional authority. Collects legitimacy from transmission fidelity.
narrative_ontology:constraint_stakeholder(transmission_fidelity_mechanism, rabbinic_interpretive_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transmission_fidelity_mechanism, rabbinic_interpretive_authority, beneficiary).

% Administers the transmission mechanism through curriculum structure, manuscript preservation, and pedagogical protocols. Benefits from the constraint as it provides core curriculum content (Talmudic tractates Zevachim and Menachot are central texts) and structures the yeshiva educational model (detailed legal analysis of non-performable laws trains analytical skills). Constrained exit: abandoning sacrifice law study would require restructuring the entire curriculum and would undermine the institution's claim to preserve unbroken tradition.
narrative_ontology:constraint_stakeholder(transmission_fidelity_mechanism, yeshiva_institutional_structure, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(transmission_fidelity_mechanism, yeshiva_institutional_structure, beneficiary).

% Studies sacrifice law as part of Torah study obligation. Bears the time and cognitive cost of mastering detailed procedural law for a practice that cannot be performed. Identity-locked: the student's religious identity is constituted through the halakhic framework; exit would require abandoning Orthodox Judaism entirely, not just this area of study. Experiences the obligation as divine command, not as institutional extraction. The 'cost' is the study burden, but from the internal perspective this is mitzvah fulfillment, not extraction.
narrative_ontology:constraint_stakeholder(transmission_fidelity_mechanism, observant_student, payer,
    powerless, biographical, identity_locked, local).

% Organized groups (Temple Institute, other messianic movements) that view the transmission mechanism as instrumental preparation for Third Temple rebuilding. Benefits from the constraint as it maintains operational knowledge for hypothetical restoration. Mobile exit: could abandon the restoration project and reframe sacrifice law as historical, but chooses not to because messianic theology is central to their identity. Collects legitimacy and institutional purpose from the transmission mechanism.
narrative_ontology:constraint_stakeholder(transmission_fidelity_mechanism, messianic_restoration_coalition, beneficiary,
    organized, civilizational, mobile, global).

% Has exited the binding-obligation frame; treats sacrifice law as historical archive rather than active halakha. Observes the Orthodox transmission mechanism from outside and sees it as vestigial ritual maintained for institutional reasons. Mobile exit: Reform Judaism made the collective decision to exit this constraint in the 19th century, reframing sacrifice as ancient practice incompatible with modern ethics. Neither collects from nor pays into the constraint.
narrative_ontology:constraint_stakeholder(transmission_fidelity_mechanism, reform_jewish_observer, observer,
    moderate, biographical, mobile, regional).

% Examines the transmission mechanism as a commitment system phenomenon. Sees the high theater ratio (detailed study of non-performable law), the low extraction (genuine scholarly coordination), and the mandatrophy (original function atrophied, institutional maintenance persists). Recognizes that different readings of the sacrifice obligation kernel produce structurally distinct constraints. Neither collects from nor pays into the constraint; observes its operation across all perspectives.
narrative_ontology:constraint_stakeholder(transmission_fidelity_mechanism, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves detailed legal knowledge of Temple sacrifice procedures across generational time; maintains institutional continuity and rabbinic authority through unbroken-transmission doctrine; structures yeshiva curriculum and pedagogical method (Talmudic legal analysis).
% TRANSFER_FUNCTION: Transfers legitimacy and institutional authority from the Temple-era priesthood to the rabbinic interpretive tradition; transfers curriculum content and pedagogical structure to yeshiva institutions; transfers time and cognitive effort from students to the study of non-performable law.
% ABSENT_VOICES: Those who would argue the transmission mechanism is pure theater (secular critics, some Reform voices) are largely absent from the Orthodox institutional conversation that maintains the constraint. Also absent: voices that would argue for abandoning sacrifice law study entirely as obsolete (this position exists in Reform Judaism but is not represented in Orthodox discourse). The unanimity within Orthodox Judaism about the value of sacrifice law study arises partly because dissenting seats (those who see it as vestigial) have exited to other denominations.
% DISAPPEARANCE_RATIONALE: If the transmission mechanism disappeared overnight, the Orthodox world would experience significant disruption: loss of core curriculum content (Zevachim and Menachot are central Talmudic tractates), undermining of the unbroken-transmission doctrine (if sacrifice law is abandoned, what else in the tradition is negotiable?), and loss of a key institutional legitimation mechanism (rabbinic authority to redefine mitzvah modality). However, from a secular or Reform perspective, nothing of functional importance would change — the laws being studied cannot be performed anyway, so their loss would be purely symbolic/cultural. The verdict is contested because Orthodox and non-Orthodox frameworks disagree on whether the transmission mechanism is structurally necessary (Orthodox: yes, it maintains tradition and fulfills mitzvah; non-Orthodox: no, it is cultural performance that could be replaced or abandoned).
% FOUNDING_PROBLEM: The founding problem was the destruction of the Second Temple in 70 CE, which made physical performance of sacrifice structurally impossible (no Temple, no priesthood in ritual purity, no altar). The rabbinic response was to preserve the legal corpus governing sacrifice and to develop the theological doctrine that study of sacrifice law constitutes fulfillment of the mitzvah (Menachot 110a). The transmission mechanism was built to solve two problems: (1) maintain operational knowledge in case of Temple restoration (instrumental preparation), and (2) preserve the continuity of Torah study and rabbinic authority despite the impossibility of enactment (institutional legitimation).
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's status is contested between Orthodox and non-Orthodox frameworks. Orthodox sources (rabbinic authorities, yeshiva institutions, messianic movements) attest that the problem is still live: the Temple could be rebuilt (messianic restoration), and the operational knowledge must be maintained. Non-Orthodox sources (Reform Judaism, secular Jewish scholarship, historical-critical analysis) attest that the problem is dead: 1,900 years without restoration makes the instrumental justification implausible, and the transmission mechanism persists for institutional reasons (authority maintenance, curriculum structure) rather than functional preparation. Corroboration from outside the beneficiary set: secular historians of Judaism (e.g., Jacob Neusner, Shaye Cohen) document that the study-as-fulfillment doctrine emerged as a theological accommodation to the impossibility of performance, not as preparation for restoration. Archaeological and political analysis suggests Third Temple rebuilding is structurally implausible (requires demolition of the Dome of the Rock, which would trigger regional conflict). These external sources support the 'dead' assessment, but Orthodox theology rejects this framing as category error (divine intervention can overcome structural barriers).
narrative_ontology:disappearance_verdict(transmission_fidelity_mechanism, contested).
narrative_ontology:founding_problem_status(transmission_fidelity_mechanism, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YESHIVA INSTITUTIONAL STRUCTURE (PITON) — Maintains elaborate transmission apparatus for laws that cannot be enacted. The institutional machinery (curriculum structure, adjudication protocols, manuscript preservation) persists at high fidelity despite zero performative outlet. Theater ratio is high: the detailed study and adjudication of sacrifice law serves institutional continuity and authority maintenance rather than preparation for actual performance. The institution experiences this as degraded function maintained through inertia — the original purpose (training priests for Temple service) has atrophied, but the transmission mechanism continues because it vindicates rabbinic authority and structures yeshiva pedagogy.
constraint_indexing:constraint_classification(transmission_fidelity_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: RABBINIC INTERPRETIVE AUTHORITY (ROPE) — Benefits from the transmission mechanism as it vindicates the unbroken-chain doctrine and the authority to redefine mitzvah modality. The constraint coordinates legitimate scholarly activity: preserving detailed legal knowledge demonstrates continuity with Temple-era authority and validates the claim that study constitutes fulfillment. Low extraction: the authority structure collects legitimacy from the transmission fidelity, but this is coordination of a genuine scholarly function (maintaining legal expertise) rather than pure extraction. Arbitrage exit: rabbinic authority could reframe sacrifice law as purely historical archive (as Reform Judaism has done) but chooses not to because the transmission mechanism serves institutional interests.
constraint_indexing:constraint_classification(transmission_fidelity_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: MESSIANIC RESTORATION EXPECTATION (SCAFFOLD) — Views the transmission mechanism as temporary maintenance of operational readiness. The constraint has an implicit sunset: when the Temple is rebuilt (messianic era), the detailed knowledge preserved through study will enable immediate resumption of sacrifice. From this perspective, the high-fidelity transmission is not theatrical but instrumental — maintaining capacity during a divinely ordained suspension period. The organized coalition (those holding messianic restoration theology) sees this as coordination with a clear endpoint, not degraded performance.
constraint_indexing:constraint_classification(transmission_fidelity_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: OBSERVANT STUDENT (MOUNTAIN) — Experiences the obligation to study sacrifice law as immutable divine command. Identity-locked: the student's religious identity is constituted through the halakhic framework; exit would require abandoning the entire commitment system, not just this constraint. From this perspective, the transmission mechanism is not theatrical but sacred duty — the command to study Torah includes these laws, and their non-performability is irrelevant to the obligation. The constraint appears as natural law: God commanded study of these laws, therefore study is required, regardless of whether performance is possible.
constraint_indexing:constraint_classification(transmission_fidelity_mechanism, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 5: REFORM JEWISH OBSERVER (PITON) — Sees the transmission mechanism as vestigial ritual maintained for institutional reasons. Mobile exit: Reform Judaism has already exited the binding-obligation frame, treating sacrifice law as historical archive rather than active halakha. From this perspective, the Orthodox maintenance of detailed sacrifice law study is clearly theatrical — preserving minute procedural details for a practice that will never resume (Reform theology rejects animal sacrifice even if a Temple were rebuilt). The constraint persists in Orthodox institutions not because it serves a real function but because abandoning it would undermine the unbroken-transmission narrative that grounds rabbinic authority.
constraint_indexing:constraint_classification(transmission_fidelity_mechanism, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — Recognizes the transmission mechanism as a commitment system whose primary function has atrophied while the institutional apparatus persists. The analytical perspective sees the high theater ratio clearly: 1,900 years of detailed adjudication on non-performable questions, manuscript preservation at extraordinary fidelity for laws with zero enactment pathway, and yeshiva curriculum structured around a practice that cannot be executed. The constraint's extractiveness is low because it genuinely coordinates scholarly activity and preserves cultural knowledge, but its functional justification (preparation for Temple restoration) is largely performative. The mechanism persists because it vindicates institutional authority (unbroken transmission doctrine) and structures pedagogy, not because it serves its stated purpose.
constraint_indexing:constraint_classification(transmission_fidelity_mechanism, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transmission_fidelity_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(transmission_fidelity_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(transmission_fidelity_mechanism, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(transmission_fidelity_mechanism, TR),
    TR >= 0.70.

:- end_tests(transmission_fidelity_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low. The transmission mechanism coordinates genuine scholarly activity (preserving legal expertise, maintaining cultural knowledge) and serves institutional functions (structuring pedagogy, vindicating authority). The extractiveness is not zero because the mechanism does collect institutional benefits (authority legitimation, curriculum content) that are somewhat decoupled from its stated function (preparation for Temple restoration). But there is no clear victim set: students who study sacrifice law are fulfilling a religious obligation (from the internal perspective) or engaging in voluntary cultural practice (from the external perspective), not bearing extraction. The declining trajectory reflects that as the study-as-fulfillment doctrine became more established, the gap between stated function (preparation) and actual function (study as mitzvah) narrowed, reducing the extractive component. Suppression (0.20): Low. The constraint operates primarily through identity-lock (observant students cannot exit without abandoning the halakhic framework) rather than through active coercion. Students are not forced to study sacrifice law; they do so because their religious identity commits them to Torah study, which includes these laws. The suppression is non-zero because the identity-lock is real and because institutional structures (yeshiva curriculum requirements) do enforce study, but it is substantially lower than constraints that operate through material barriers or legal coercion. Theater ratio (0.78): High. The constraint's primary activity — detailed study and adjudication of sacrifice law — is largely performative relative to its stated function (preparation for Temple restoration). The rising trajectory reflects that as the temporal distance from the Temple destruction increased, the probability of restoration decreased (from a secular analytical perspective), making the transmission mechanism increasingly theatrical. At T=0 (70 CE), many believed the Temple would be rebuilt within a generation, so the transmission had genuine instrumental value. By T=1900 (present), 1,900 years without restoration makes the instrumental justification implausible from an external perspective, though it remains live for those holding messianic theology.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how commitment system dynamics produce radically different classifications from different structural positions. The yeshiva institutional structure and the analytical observer both classify as piton — they see the constraint's primary function as atrophied and maintained for institutional reasons. But they arrive at this classification from different seats: the institution experiences its own process as degraded (the original purpose has been lost), while the analytical observer sees the degradation from outside. The rabbinic interpretive authority sees rope — the transmission mechanism coordinates legitimate scholarly activity and vindicates institutional authority, with low extraction. The messianic restoration coalition sees scaffold — the mechanism is instrumental preparation with a sunset (Temple rebuilding). The observant student sees mountain — the obligation to study is experienced as immutable divine command, with no awareness of the theatrical component. The Reform observer sees piton from a mobile exit position — having already exited the binding-obligation frame, the Orthodox maintenance of sacrifice law study appears clearly vestigial. The gap between the mountain perspective (observant student) and the piton perspectives (institutional structure, analytical observer, Reform observer) is particularly diagnostic: the same constraint appears as natural law from inside the identity-lock and as degraded performance from outside it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary structure is unusual: the primary beneficiaries (rabbinic authority, yeshiva institutions) collect legitimacy and institutional continuity from the transmission mechanism, but there is no clear victim set. The constraint does not extract from students in the way a snare extracts from its targets — students who study sacrifice law are fulfilling a religious obligation (internal perspective) or engaging in voluntary cultural practice (external perspective). The absence of victims despite the presence of beneficiaries is what produces the low extractiveness (0.15) and the piton classification rather than tangled_rope. The constraint coordinates genuine scholarly activity (preserving legal knowledge) while also serving institutional maintenance functions (vindicating authority, structuring curriculum). The directionality derivation: rabbinic authority and yeshiva institutions are beneficiaries with arbitrage/constrained exit options, producing low d values and low/negative chi (they experience the constraint as coordination). The observant student is identity-locked but not a victim — the student's d value is moderate (the constraint imposes study obligations) but not high (the student is not bearing extraction), producing moderate chi. The messianic restoration coalition is organized with mobile exit, producing low d and low chi (they see the constraint as instrumental coordination). The Reform observer has mobile exit and is not a beneficiary, producing near-zero d (they are outside the constraint's operation). The analytical observer has analytical exit and sees the full structure, producing the piton classification from the theater ratio rather than from high chi.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a resolved mandatrophy case: the original mandate (training priests for Temple service) has been completely superseded by the actual function (vindicating rabbinic authority, structuring yeshiva pedagogy, preserving cultural knowledge). The mandate-function gap is what produces the high theater ratio and the piton classification. The constraint demonstrates that mandatrophy resolution does not require the constraint to disappear — it can persist in a degraded form, maintained for reasons other than its original purpose. The transmission mechanism's extraordinary fidelity (preserving minute procedural details across 1,900 years) is the signal: if the mechanism were genuinely instrumental (preparation for restoration), the fidelity level would track the probability of restoration, declining as the temporal distance increased. Instead, fidelity has remained high or increased, suggesting that the transmission serves institutional functions (authority maintenance, curriculum structure) that are independent of the restoration probability. The study-as-fulfillment doctrine (Menachot 110a: 'One who studies the laws of sacrifice is considered as if he offered the sacrifice') is the theological accommodation that allows the constraint to persist despite the mandate-function gap: by redefining what counts as fulfillment, the doctrine transforms the constraint from a failed obligation (cannot perform sacrifice) into a successful one (study is performance). This is the commitment system's interpretive layer absorbing the impossibility of enactment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_restoration_probability,
    'Is the messianic restoration expectation (Third Temple rebuilding) a genuine structural anticipation or a theological cover story for institutional maintenance?',
    'Historical analysis of messianic movements and their relationship to sacrifice law study intensity; correlation between institutional investment in transmission fidelity and actual Temple-rebuilding efforts; comparison with other suspended-practice traditions (e.g., Samaritan sacrifice, which resumed when possible)',
    'If genuine anticipation: scaffold classification is correct — the transmission mechanism is instrumental preparation with a real sunset. If cover story: piton classification is correct — the mechanism persists for institutional reasons while claiming a functional justification that will never materialize.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_restoration_probability, conceptual, 'Whether messianic restoration expectation is structural or theatrical').

omega_variable(
    study_as_fulfillment_doctrine_status,
    'Does the Talmudic doctrine that study of sacrifice law constitutes fulfillment of the mitzvah represent a genuine halakhic transformation (rabbinic authority to redefine performance modality) or a theological accommodation to impossibility?',
    'Comparative analysis with other transformed mitzvot; examination of whether the study-as-fulfillment doctrine is applied consistently or only to non-performable commandments; historical analysis of when and why this doctrine emerged',
    'If genuine transformation: the constraint has near-zero extractiveness (study IS the mitzvah, not a substitute). If accommodation: the constraint has moderate extractiveness (the obligation remains unfulfilled, and study is a coping mechanism for structural impossibility).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_fulfillment_doctrine_status, conceptual, 'Whether study-as-fulfillment is halakhic transformation or accommodation').

omega_variable(
    transmission_fidelity_threshold,
    'What level of transmission fidelity is functionally necessary for hypothetical Temple restoration versus what level is maintained for institutional authority reasons?',
    'Expert assessment of minimum knowledge required to resume sacrifice (basic procedures, species identification, ritual purity rules) versus actual curriculum scope (minute details of edge cases, conflicting opinions, theoretical scenarios); comparison with other suspended-then-resumed practices',
    'If current fidelity exceeds functional necessity by wide margin: theater ratio is even higher than 0.78, and piton classification is strengthened. If current fidelity matches functional necessity: scaffold classification is strengthened (the transmission is genuinely instrumental).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_fidelity_threshold, empirical, 'Functional necessity threshold for transmission fidelity').

omega_variable(
    cs_framing_under_determination,
    'Is the kernel ''the halakhic obligation to perform Temple sacrifice'' or ''the rabbinic authority to redefine mitzvah modality''?',
    'The study-as-exercise reading treats rabbinic interpretive authority as the kernel (the authority to declare study = performance is what persists); the performance-only reading treats the Torah sacrifice command as the kernel (the physical obligation is what persists, unfulfilled). These framings produce different cs_pattern classifications: interpretive_capture (rabbinic authority kernel) versus suspended_obligation (sacrifice command kernel).',
    'If rabbinic authority is the kernel: the transmission mechanism is the interpretive layer that absorbs the impossibility of performance, and the constraint is a successful commitment system (authority persists by redefining what counts as fulfillment). If sacrifice command is the kernel: the transmission mechanism is theatrical maintenance of a command that cannot be obeyed, and the constraint is a failed commitment system (the kernel''s demand cannot be met, but the system persists by claiming study is sufficient).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cs_framing_under_determination, conceptual, 'Kernel identity: sacrifice obligation or rabbinic authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transmission_fidelity_mechanism, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trans_fid_theater_70ce, transmission_fidelity_mechanism, theater_ratio, 0, 0.4).
narrative_ontology:measurement(trans_fid_theater_1200ce, transmission_fidelity_mechanism, theater_ratio, 500, 0.55).
narrative_ontology:measurement(trans_fid_theater_1700ce, transmission_fidelity_mechanism, theater_ratio, 1000, 0.68).
narrative_ontology:measurement(trans_fid_theater_2100ce, transmission_fidelity_mechanism, theater_ratio, 1400, 0.75).
narrative_ontology:measurement(trans_fid_theater_current, transmission_fidelity_mechanism, theater_ratio, 1900, 0.78).

% Extraction over time
narrative_ontology:measurement(trans_fid_extract_70ce, transmission_fidelity_mechanism, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(trans_fid_extract_1200ce, transmission_fidelity_mechanism, base_extractiveness, 500, 0.22).
narrative_ontology:measurement(trans_fid_extract_1700ce, transmission_fidelity_mechanism, base_extractiveness, 1000, 0.18).
narrative_ontology:measurement(trans_fid_extract_2100ce, transmission_fidelity_mechanism, base_extractiveness, 1400, 0.16).
narrative_ontology:measurement(trans_fid_extract_current, transmission_fidelity_mechanism, base_extractiveness, 1900, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transmission_fidelity_mechanism, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel. The sacrifice obligation kernel decomposes into four structurally distinct constraints (study-as-exercise, performance-only, messianic-suspension, symbolic-archive), each with different beneficiary/victim structures and extractiveness profiles. The study-as-exercise reading (this constraint) has low extractiveness because it treats study as legitimate fulfillment; the performance-only reading would have high extractiveness because it treats the obligation as unfulfilled; the messianic-suspension reading has low extractiveness because it treats the obligation as in abeyance; the symbolic-archive reading has zero extractiveness because it treats the obligation as non-binding. These are not different perspectives on the same constraint — they are different constraints instantiated by different readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
