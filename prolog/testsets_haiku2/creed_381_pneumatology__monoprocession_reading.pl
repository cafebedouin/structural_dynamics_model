% ============================================================================
% CONSTRAINT STORY: creed_381_pneumatology__monoprocession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creed_381_pneumatology__monoprocession_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: creed_381_pneumatology__monoprocession_reading
 *   human_readable: Monoprocession Reading of the 381 Creed: Spirit from Father Alone, Ecumenical Amendment Lock
 *   domain: ecclesiastical/theological
 *
 * SUMMARY:
 *   The monoprocession reading of the contested 381 creed asserts that the
 *   Spirit proceeds from the Father alone (rejecting the later Western
 *   Filioque addition) and that the creed itself is inviolable without full
 *   ecumenical consent. Under this reading, the constraint operates as a
 *   decentralized polity lock: no single see (Roman, Constantinople, or
 *   other) can unilaterally amend doctrine for the whole Church. The Eastern
 *   Orthodox understanding of conciliar supremacy and the equality of the
 *   five patriarchs is vindicated. Western unilateral innovation (the
 *   Filioque, added to the creed in the 9th century without Eastern consent)
 *   is branded a breach that fragmented communion. The constraint thus
 *   preserves a power structure in which Eastern autonomous churches retain
 *   structural veto over doctrinal change; Western institutional hierarchy
 *   (papal magisterium, Western councils claiming universal scope) loses
 *   unilateral amendment authority. This reading extracts from Western
 *   institutional ambition while benefiting Eastern conciliar polity.
 *
 * KEY AGENTS:
 *   - Eastern autocephalous churches: custodians and enforcement seats; defend the creed-lock and veto unilateral Western amendment
 *   - Western unilateral innovators (papal magisterium, regional Western councils): pay the cost of constrained doctrinal authority; lose ability to clarify doctrine without East's consent
 *   - Conciliar ecumenical structure: institutional gate for amendment; the reading invests it with near-absolute gatekeeping power
 *   - Lay faithful (Eastern and Western): identity-locked observers; inherit the constraint's liturgical/doctrinal consequences
 *   - Reform theologians and critical scholars: excluded from amendment gate; their voices absent from the binding conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creed_381_pneumatology__monoprocession_reading, 0.68).
domain_priors:suppression_score(creed_381_pneumatology__monoprocession_reading, 0.72).
domain_priors:theater_ratio(creed_381_pneumatology__monoprocession_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creed_381_pneumatology__monoprocession_reading, tangled_rope).
narrative_ontology:human_readable(creed_381_pneumatology__monoprocession_reading, "Monoprocession Reading of the 381 Creed: Spirit from Father Alone, Ecumenical Amendment Lock").
narrative_ontology:topic_domain(creed_381_pneumatology__monoprocession_reading, "ecclesiastical/theological").

domain_priors:requires_active_enforcement(creed_381_pneumatology__monoprocession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(creed_381_pneumatology__monoprocession_reading, '1e539c7b-e233-464d-bfba-fea068303d80').
narrative_ontology:cs_kernel_codification('1e539c7b-e233-464d-bfba-fea068303d80', fixed_text).
narrative_ontology:cs_authority_grounding('1e539c7b-e233-464d-bfba-fea068303d80', lineage).
narrative_ontology:cs_interpretation_layer_present('1e539c7b-e233-464d-bfba-fea068303d80').
narrative_ontology:cs_reading_relation('1e539c7b-e233-464d-bfba-fea068303d80', creed_381_pneumatology__filioque_reading, forecloses).
narrative_ontology:cs_reading_relation('1e539c7b-e233-464d-bfba-fea068303d80', creed_381_pneumatology__ecumenical_reunion_reading, influences).
narrative_ontology:cs_axiom('1e539c7b-e233-464d-bfba-fea068303d80', foundational, conciliar_amendment_exclusivity).
narrative_ontology:cs_axiom_status(conciliar_amendment_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('1e539c7b-e233-464d-bfba-fea068303d80', conciliar_amendment_exclusivity, conventional).
narrative_ontology:cs_axiom('1e539c7b-e233-464d-bfba-fea068303d80', foundational, unilateral_innovation_constitutes_breach).
narrative_ontology:cs_axiom_status(unilateral_innovation_constitutes_breach, holdable).
narrative_ontology:cs_axiom_grounding('1e539c7b-e233-464d-bfba-fea068303d80', unilateral_innovation_constitutes_breach, deontological).
narrative_ontology:cs_reference_frame('1e539c7b-e233-464d-bfba-fea068303d80', conciliar_pneumatological_supremacy).
narrative_ontology:cs_drift_state('1e539c7b-e233-464d-bfba-fea068303d80', contemporary_filioque_entrenchment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1e539c7b-e233-464d-bfba-fea068303d80', '').
narrative_ontology:cs_kernel_id(creed_381_pneumatology__monoprocession_reading, creed_381_pneumatology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__monoprocession_reading, eastern_autocephalous_churches).
narrative_ontology:constraint_victim(creed_381_pneumatology__monoprocession_reading, western_unilateral_innovators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(creed_381_pneumatology__monoprocession_reading, papal_western_magisterium).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defend the Council of 381's pneumatology (Spirit from Father alone) as fixed doctrine inviolable without ecumenical consensus. Through the Orthodox communion's institutional continuity, they function as custodians and enforcement seats: they veto unilateral Western amendment through continued non-recognition of doctrine added without their consent. They benefit from the constraint because it prevents Western liturgical/doctrinal drift from fracturing the whole Church without their participation.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, eastern_autocephalous_churches, beneficiary,
    organized, civilizational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__monoprocession_reading, eastern_autocephalous_churches, agenda_setter).

% Seek to clarify or extend Trinitarian doctrine unilaterally (historically, the Filioque addition in the 9th–11th centuries). The constraint locks them into either ecumenical consensus-building (high coordination cost) or schismatic breach of communion (identity rupture). Their unilateral doctrinal authority, which papal and Western conciliar traditions claimed, is constrained by the reading's enforcement: amendment without Eastern consent is branded breach, delegitimizing the innovation and fragmenting the Church.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, western_unilateral_innovators, payer,
    institutional, civilizational, constrained, continental).

% The institutional framework that enforces the constraint: ecumenical councils (as the reading understands them) are the sole legitimate amendment venue for creeds. No single patriarchate or regional see can amend doctrine for the whole Church. The reading invests conciliar authority with near-absolute gatekeeping power over doctrinal change.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, conciliar_ecumenical_structure, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(creed_381_pneumatology__monoprocession_reading, conciliar_ecumenical_structure).

% In the monoprocession reading, the papal claim to autonomous doctrinal authority (whether through papal infallibility or conciliar definitions that treat papal leadership as decisive) is structurally blocked. The reading denies the West unilateral legislative power over creeds binding on the whole Church. They are excluded from the amendment gate if they act unilaterally; they become payers (bearing the cost of schism or prolonged non-recognition) if they innovate without consensus.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, papal_western_magisterium, payer,
    institutional, civilizational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__monoprocession_reading, papal_western_magisterium, excluded).

% The First Council of Constantinople (381) is canonized as inviolable under the monoprocession reading: its pneumatology stands without revision. The councils themselves (as readings of conciliar supremacy understand them) are vindicated as supreme doctrinal authority.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, early_ecumenical_councils, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(creed_381_pneumatology__monoprocession_reading, early_ecumenical_councils).

% Inherit the doctrinal and liturgical consequences of the constraint. Eastern believers live in liturgies that preserve monoprocession without filioque; Western believers either assent to Filioque additions authorized unilaterally by their see, or face identity rupture if they reject their own communion's doctrinal framework. Their exit from either church is identity-locked: leaving the Church means abandoning not just doctrine but the relational and epistemic fabric of their faith community.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, eastern_and_western_lay_faithful, observer,
    powerless, biographical, identity_locked, universal).

% Theologians who question whether the 381 pneumatology is empirically defensible or whether unilateral Western innovation (the Filioque) represents legitimate development rather than breach, find themselves excluded from the amendment gate. They cannot change doctrine without ecumenical consensus, and ecumenical consensus is structurally biased toward the 381 fixed point. Their scholarly voices are not in the institutional amendment conversation.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, reform_movements_and_critical_theologians, excluded,
    powerless, biographical, trapped, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the global Church under a single, unified creed (the monoprocession pneumatology of 381) by preventing doctrinal fragmentation through unilateral regional innovation. Requires all doctrinal amendments to pass through conciliar consensus rather than unilateral sees.
% TRANSFER_FUNCTION: Transfers doctrinal authority from unilateral Western institutional hierarchies (papacy, Western councils) to distributed Eastern autonomous churches holding collective veto. Moves amendment power from concentrated to diffuse. Directly transfers the capacity to define doctrine for the whole Church from Rome to the ecumenical council structure (in which Eastern churches hold veto by the monoprocession reading's own framing).
% ABSENT_VOICES: Western theologians advocating doctrinal development (Scholastics defending the Filioque as legitimate clarification, later Marian dogmatists, soteriological innovators); reform movements questioning whether the 381 pneumatology was culturally contingent; lay faithful in Western churches who accept the Filioque and do not view it as breach; critical historians who treat both monoprocession and Filioque as regional theological expressions rather than universal truth claims. These voices are excluded from the amendment gate because the reading concentrates amendment authority in ecumenical councils (which have been rare and unequal in representation since 381) and denies amendment legitimacy to unilateral sees or theological guilds.
% DISAPPEARANCE_RATIONALE: If this constraint (the monoprocession reading and its ecumenical veto structure) vanished, the ecclesiastical landscape would reorganize around either: (1) explicit regional theological difference without the fiction of shared creed (ecumenicism abandons unified doctrine), or (2) Western unilateral doctrinal definitions without Eastern concurrence (filioque, Marian dogmas, soteriological definitions become binding in Western churches through papal or conciliar authority alone, without Eastern consent). Either path reshapes East-West relations fundamentally. The constraint's presence maintains the possibility of future reconciliation under shared 381 creed; its disappearance would resolve that possibility definitively in favor of either acknowledged pluralism or Western precedence.
% FOUNDING_PROBLEM: After the First Council of Constantinople (381), the Church faced a structural risk: if any single see (particularly Rome, with its institutional resources and claims to primacy) could unilaterally amend creeds binding on the whole Church, doctrinal hegemony by the strongest see would fragment the Church and destroy conciliar authority. The monoprocession reading embeds in the creed itself the principle that amendment requires ecumenical consensus, making the creed a structural guard against unilateral institutional power.
% FOUNDING_PROBLEM_CORROBORATION: Eastern Orthodox and Oriental Orthodox communions attest the founding problem is live: the Western Filioque addition in the 9th–11th centuries without Eastern consent proves that unilateral innovation destroys communion and that the creed-lock is necessary to prevent recurrence. Western Catholic and Protestant institutional histories attest that doctrinal development is legitimate and that the Filioque represents authoritative clarification, not breach—the founding problem was solved by conciliar discipline and papal infallibility, which they claim correctly distinguish true development from false innovation. Academic historians and patristic scholars (independent observers) attest that the 381 Council's own canons on amendment are ambiguous, that the claim to absolute immutability was a later Eastern construction, and that the founding problem's true nature (how to balance conciliar authority with regional theological expression) remains fundamentally unsettled.
narrative_ontology:disappearance_verdict(creed_381_pneumatology__monoprocession_reading, world_rearranges).
narrative_ontology:founding_problem_status(creed_381_pneumatology__monoprocession_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(creed_381_pneumatology__monoprocession_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(creed_381_pneumatology__monoprocession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(creed_381_pneumatology__monoprocession_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creed_381_pneumatology__monoprocession_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(creed_381_pneumatology__monoprocession_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(creed_381_pneumatology__monoprocession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint transfers doctrinal authority from the West to the East by making unilateral Western innovation illegitimate without Eastern consent—a structural redistribution of power. Suppression is correspondingly high (0.72): the constraint requires active enforcement through ecclesiastical non-recognition of unilateral Western amendments, institutional maintenance of the schism as a standing refusal, and canonical denial of communion to those who innovate without consensus. Theater is moderate (0.41): while the reading has genuine ecumenical and conciliar justifications grounded in patristic tradition and the canonical structure of the early councils, a significant portion of enforcement activity is performative—the regular reiteration that the Filioque is heretical, the symbolic renewal of the creed at each liturgy, the canonical anathematizing of unilateral innovation. The constraint's persistence relies partly on the theater of ecumenical reconciliation possibility (the false hope that Filioque could be dropped and union restored) to maintain the Eastern veto. The measurement series trace extraction rising steeply from interval start (0.52) to the mid-point (0.66), then plateauing (0.68)—reflecting the historical trajectory from the Filioque's initial Western insertion (low enforcement needed, filioque not yet seen as permanent) through the period of active schism-widening (high suppression, extraction rising as the East weaponizes the creed-lock against Western dominance), to the modern era where the constraint stabilizes as a structural feature of East-West division.
 *
 * PERSPECTIVAL GAP:
 *   The Eastern institutional seat and the Western institutional seat compute radically different per-seat types. The engine should compute the Eastern churches as seeing a Rope (coordination with genuine beneficiary status) while the Western seat computes as seeing a Snare (extraction with constrained exit and no real benefit). This divergence is the point: the same constraint structure looks like legitimate conciliar governance from one side and unjust Western exclusion from the other.
 *
 * DIRECTIONALITY LOGIC:
 *   The Eastern churches are net beneficiaries (d near 0.2): they gain structural veto over Western innovation, their conciliar polity model is vindicated as the correct governance frame, and they bear the cost of maintaining the schism only through symbolic enforcement (non-communion) rather than active suppression. Their exit is mobile—they could unilaterally accept the Filioque tomorrow and end the schism, but doing so would abandon the reading's core claim that unilateral innovation is illegitimate; the exit cost is ideological, not structural. Western institutions are net targets (d near 0.85): they lose unilateral doctrinal authority, bear the cost of either ecumenical consensus-building (slow, expensive, requires conceding points) or schismatic breach (losing the claim to universal authority they historically asserted). Their exit is more constrained—papal and Western conciliar traditions are so invested in the prerogative of doctrinal clarification that abandoning it would dissolve the institutional identity. The lay faithful are closer to symmetric (d near 0.5): they gain a unified creed-anchor and an institution that defends unity as sacred, but they pay the cost in separated communion and liturgical difference. Their exit from either church is identity-locked—leaving means abandoning not just doctrine but kinship, ritual, and the interpretive framework that makes the world comprehensible.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is highly contested: Eastern churches attest that unilateral Western innovation destroys communion and that the 381 creed-lock prevents recurrence; Western churches and critical historians attest that doctrinal development is legitimate and the Filioque was a theological clarification, not a breach. The disappearance_verdict is world_rearranges, which signals that the constraint's removal would significantly alter the ecclesiastical landscape (either forcing Western churches to abandon unilateral authority or forcing Eastern churches to accept the Filioque and Western doctrinal precedence). The founding_problem_status is contested. Under the monoprocession reading, mandatrophy has NOT occurred—the founding problem is still live because Western unilateral innovation continues to threaten Church unity (the constraint correctly identifies and gates the threat). But the Western institutional perspective would attest mandatrophy: the founding problem was preventing chaos through decentralized governance, but modern conditions allow doctrinal diversity and communion without total agreement, so the creed-lock creates false unity (theater) rather than real coordination. The constraint classification should hold as tangled_rope: it genuinely coordinates around a unified creed (beneficiary function) AND asymmetrically extracts from Western unilateral institutions (victim function) AND requires active enforcement (conciliar gatekeeping, canonical non-recognition, institutional maintenance of the schism). The claim and metrics align: the monoprocession reading is a Tangled Rope from the Eastern seat (coordination they benefit from, asymmetric veto they hold), and a Snare from the Western seat (extraction they pay, no real coordination benefit, constrained exit). The per-seat divergence is the signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_liveness_contest,
    'Is the founding problem (preventing unilateral doctrinal innovation that fractures communion) still live, or is it dead and the constraint now a zombie performance?',
    'Historical analysis of post-Filioque Western doctrinal innovations (Immaculate Conception, Assumption, etc.) and their Eastern reception: did they cause additional schism, or did the East learn to tolerate Western development? Counterfactual: if the monoprocession reading were formally abandoned, would the Western churches proceed to new unilateral doctrinal definitions, or has Western doctrinal adventurism already subsided for other reasons?',
    'If the founding problem is dead, the constraint reclassifies from Tangled Rope to Piton (preserved by performance and identity-lock rather than by living coordination need). If it is live, the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_liveness_contest, conceptual, 'Whether the constraint addresses a living threat to Church unity or a historical artifact maintained by institutional inertia.').

omega_variable(
    structural_vs_internalized_suppression,
    'Is the measured suppression (0.72) primarily structural (institutional veto and canonical barriers maintained by the Eastern hierarchy) or internalized (Western institutional guilt and the carry-forward of shame for unilateral innovation)?',
    'Historical and ethnographic: trace Western institutional rhetoric across centuries—are they defending the Filioque as true doctrine, or apologizing for it as a historical mistake? Conduct counterfactual: if Eastern churches unilaterally dropped the excoriation of the Filioque but Western churches dropped it anyway, structural suppression is low; if Western institutions continue defending it, suppression is internalized.',
    'If structural, the constraint remains enforceable by Eastern veto even if Western commitment weakens. If internalized, the constraint''s persistence depends on Western institutional guilt persisting; a generational shift in Western consciousness could erode it without formal Eastern concession.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'The mechanism sustaining the measured suppression: institutional enforcement or inherited guilt.').

omega_variable(
    kernel_reading_classification_ambiguity,
    'Is this constraint accurately described as the monoprocession reading''s instantiation, or does it conflate two distinct readings: (1) pneumatological realism (the Spirit really does proceed from Father alone, period), and (2) governance ecclesiology (unilateral amendment is illegitimate without ecumenical consent)?',
    'Theological analysis: can one hold the Filioque as theologically true while also accepting the monoprocession reading''s governance claim (no unilateral amendment)? The Eastern Fathers—do they ground their opposition to the Filioque in metaphysical realism about Trinitarian procession, or in the principle that doctrine is set by councils, not unilateral sees? If the former, the two are inseparable; if the latter, they are conceptually distinct and could, in principle, be held separately.',
    'If they are separable, the constraint story should be decomposed into two: pneumatological (Spirit''s real procession) and ecclesiological (amendment authority). If inseparable, the monoprocession reading correctly unifies them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_classification_ambiguity, conceptual, 'Whether the monoprocession reading is a single ε-invariant constraint or a conflation of two distinct structures.').

omega_variable(
    conciliar_authority_definition_circularity,
    'Does the monoprocession reading define ecumenical councils in a way that circularly justifies the creed-lock? If councils are defined as decisions that carry ecumenical consent, and ecumenical consent is defined as agreement across the five patriarchs, then by construction no council can amend the 381 creed (because amending it would by definition lack ecumenical consent from all five).',
    'Historical and definitional: What is the independent definition of an ecumenical council that the monoprocession reading draws on? If it is circular (council = decision with ecumenical consent, therefore no council can alter consensus positions), the reading has authored an unfalsifiable thesis. If the definition of council is independent (formal ecumenical assembly, majority vote counted in a specified way), then amendment is logically possible.',
    'If circular, the constraint''s claim to rationality is weakened; it becomes a performative assertion (''the creed cannot be amended because amendment would violate the definition of amendment'') rather than a substantive governance principle. If independent, the constraint is substantive but falsifiable (a future ecumenical council could, in principle, amend the creed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conciliar_authority_definition_circularity, conceptual, 'Whether the creed-lock rests on substantive governance principle or circular definition.').

omega_variable(
    contention_over_reading_identity,
    'Does the monoprocession reading belong to the sibling set defined by the kernel (creed_381_pneumatology), or is it better understood as a distinct constraint about conciliar governance that happens to be grounded in 381?',
    'Semantic analysis: the three sibling readings all concern the TRUTH of what the Spirit''s procession IS (monoprocession vs. Filioque vs. both-valid). The monoprocession reading''s core claim is about AUTHORITY (councils amend, singles do not). The truth claim and the authority claim are related but distinct. A fourth reading—the governance_supremacy_reading—might assert ''unilateral amendment is illegitimate regardless of pneumatological content'' and would cut across the pneumatological axis entirely.',
    'If the monoprocession reading is misaligned with its kernel (pneumatology) and should be reclassified as governance_supremacy, then the constraint story''s kernel_id and sibling relations are misdeclared. The sibling relationships (forecloses/coexists/influences) would need to be re-examined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contention_over_reading_identity, conceptual, 'Whether this constraint is correctly classified as a reading of the pneumatological kernel or should be reframed as a governance-authority constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creed_381_pneumatology__monoprocession_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cree_tr_t0, creed_381_pneumatology__monoprocession_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cree_tr_t8, creed_381_pneumatology__monoprocession_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(cree_tr_t16, creed_381_pneumatology__monoprocession_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(cree_tr_t24, creed_381_pneumatology__monoprocession_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(cree_tr_t32, creed_381_pneumatology__monoprocession_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(cree_tr_t40, creed_381_pneumatology__monoprocession_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(cree_tr_t50, creed_381_pneumatology__monoprocession_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(cree_be_t0, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(cree_be_t8, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(cree_be_t16, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(cree_be_t24, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(cree_be_t32, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(cree_be_t40, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(cree_be_t50, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cree_su_t0, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(cree_su_t8, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(cree_su_t16, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(cree_su_t24, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(cree_su_t32, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(cree_su_t40, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(cree_su_t50, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creed_381_pneumatology__monoprocession_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(creed_381_pneumatology__monoprocession_reading, 0.12).
narrative_ontology:affects_constraint(creed_381_pneumatology__monoprocession_reading, creed_381_pneumatology__filioque_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__monoprocession_reading, creed_381_pneumatology__ecumenical_reunion_reading).

% DUAL FORMULATION NOTE:
% The monoprocession_reading, filioque_reading, and ecumenical_reunion_reading form a constraint family decomposed from the contested kernel 'creed_381_pneumatology'. All three stories share the referent (the 381 creed and the question of amendment authority) but instantiate radically different ε values and beneficiary/victim structures because they are different readings of what the creed means and who has legitimate authority to clarify it. The monoprocession reading asserts high ε (the creed-lock extracts from Western unilateral authority). The Filioque reading asserts low ε (Western amendment is legitimate clarification, not extraction). The reunion reading asserts medium ε (bilateral negotiation is both coordinated and asymmetrically costly to those who must negotiate). All three should be generated as separate constraint stories with their own metrics, stakeholder surfaces, and omegas; they are linked via network.affects_constraints to show their interdependence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
