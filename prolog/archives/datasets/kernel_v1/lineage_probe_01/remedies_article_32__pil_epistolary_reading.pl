% ============================================================================
% CONSTRAINT STORY: remedies_article_32__pil_epistolary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remedies_article_32__pil_epistolary_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: remedies_article_32__pil_epistolary_reading
 *   human_readable: Article 32 PIL Epistolary Reading: Standing Relaxed to Proxy Advocates
 *   domain: legal/constitutional/remedial
 *
 * SUMMARY:
 *   Article 32 of the Indian Constitution grants the court power to issue
 *   prerogative writs (habeas corpus, mandamus, prohibition, quo warranto,
 *   certiorari) for the enforcement of fundamental rights. The
 *   pil_epistolary_reading relaxes the classical standing requirement: a
 *   court no longer requires the plaintiff to be the direct victim of the
 *   violation. Instead, 'anyone' can file a petition on behalf of 'anyone' —
 *   a postcard from a prisoner can become a petition for remedy, standing is
 *   'relaxed to whoever speaks for the voiceless.' This reading has
 *   democratized access to constitutional remedy, enabling mass litigation on
 *   behalf of the unrepresented poor (bus passengers seeking safety
 *   standards, forest dwellers resisting environmental destruction, detainees
 *   alleging torture). The reading is one instantiation of a contested
 *   kernel: Article 32 itself. Other readings
 *   (pil_overreach_critique_reading, writ_arsenal_reading) interpret the same
 *   text differently, producing different constraint structures. This story
 *   is the pil_epistolary_reading only — not a synthesis of all readings, and
 *   not a hedge across interpretations. The constraint described here is the
 *   epistolary reading's specific structural claims: standing suppression
 *   removed, beneficiary is the unrepresented poor, victim set is classical
 *   adversarial doctrine, extractiveness comes from proxy gatekeeping and
 *   judicial administration of remedies.
 *
 * KEY AGENTS:
 *   - Imprisoned or Isolated Victim: Primary beneficiary via proxy (powerless/trapped) — can access remedy only through the standing relaxation; no other pathway exists
 *   - Proxy Advocate (Lawyer, NGO, Journalist): Primary beneficiary (powerful/arbitrage) — can file petitions without a paying client; coordinates unorganized suffering into actionable claims
 *   - Social Movement Ecosystem: Secondary beneficiary and victim (organized/constrained) — benefits from standing relaxation but experiences extraction through judicial administration of political demands
 *   - Wronged but Unorganized Constituency: Beneficiary and victim (moderate/constrained) — gains access to remedy but loses autonomy over claim framing to the proxy
 *   - Adversarial-Form Purism / Classical Doctrine: Victim (institutional/trapped) — standing requirement and party-plaintiff doctrine are overridden by the constraint
 *   - Judicial System: Beneficiary and victim (institutional/constrained) — expands power and legitimacy but takes on non-traditional administrative burdens
 *   - Analytical Observer: Constitutional democracy perspective (analytical/analytical) — sees constraint as enabling access-to-justice promise but also as subordinating democracy to judicial wisdom
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remedies_article_32__pil_epistolary_reading, 0.38).
domain_priors:suppression_score(remedies_article_32__pil_epistolary_reading, 0.42).
domain_priors:theater_ratio(remedies_article_32__pil_epistolary_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remedies_article_32__pil_epistolary_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(remedies_article_32__pil_epistolary_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(remedies_article_32__pil_epistolary_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remedies_article_32__pil_epistolary_reading, tangled_rope).
narrative_ontology:human_readable(remedies_article_32__pil_epistolary_reading, "Article 32 PIL Epistolary Reading: Standing Relaxed to Proxy Advocates").
narrative_ontology:topic_domain(remedies_article_32__pil_epistolary_reading, "legal/constitutional/remedial").

domain_priors:requires_active_enforcement(remedies_article_32__pil_epistolary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remedies_article_32__pil_epistolary_reading, '966f4498-6549-4267-96f2-f6c0d66afc03').
narrative_ontology:cs_kernel_codification('966f4498-6549-4267-96f2-f6c0d66afc03', fixed_text).
narrative_ontology:cs_authority_grounding('966f4498-6549-4267-96f2-f6c0d66afc03', lineage).
narrative_ontology:cs_interpretation_layer_present('966f4498-6549-4267-96f2-f6c0d66afc03').
narrative_ontology:cs_reading_relation('966f4498-6549-4267-96f2-f6c0d66afc03', remedies_article_32__pil_overreach_critique_reading, coexists_with).
narrative_ontology:cs_reading_relation('966f4498-6549-4267-96f2-f6c0d66afc03', remedies_article_32__writ_arsenal_reading, influences).
narrative_ontology:cs_axiom('966f4498-6549-4267-96f2-f6c0d66afc03', foundational, standing_relaxation_serves_access_to_remedy).
narrative_ontology:cs_axiom_status(standing_relaxation_serves_access_to_remedy, holdable).
narrative_ontology:cs_axiom_grounding('966f4498-6549-4267-96f2-f6c0d66afc03', standing_relaxation_serves_access_to_remedy, deontological).
narrative_ontology:cs_axiom('966f4498-6549-4267-96f2-f6c0d66afc03', foundational, proxy_advocacy_legitimate_under_necessity).
narrative_ontology:cs_axiom_status(proxy_advocacy_legitimate_under_necessity, holdable).
narrative_ontology:cs_axiom_grounding('966f4498-6549-4267-96f2-f6c0d66afc03', proxy_advocacy_legitimate_under_necessity, instrumental).
narrative_ontology:cs_reference_frame('966f4498-6549-4267-96f2-f6c0d66afc03', access_to_remedy_as_fundamental_right).
narrative_ontology:cs_drift_state('966f4498-6549-4267-96f2-f6c0d66afc03', post_pil_maturation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('966f4498-6549-4267-96f2-f6c0d66afc03', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(remedies_article_32__pil_epistolary_reading, remedies_article_32).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remedies_article_32__pil_epistolary_reading, unrepresented_poor).
narrative_ontology:constraint_beneficiary(remedies_article_32__pil_epistolary_reading, proxy_advocates).
narrative_ontology:constraint_victim(remedies_article_32__pil_epistolary_reading, adversarial_form_purism).
narrative_ontology:constraint_victim(remedies_article_32__pil_epistolary_reading, party_plaintiff_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE IMPRISONED PETITIONER (SNARE) — Cannot hire counsel; cannot travel to court; cannot organize standing themselves. The postcard-to-petition pathway is the ONLY exit from torture or illegal detention. Extreme suppression: no legal remedy exists except through the goodwill of a lawyer who will take the case pro bono. Yet this perspective benefits from standing relaxation — the constraint removes the barrier to access. The experience is paradoxical: snare in capacity (trapped without external advocate), rope in outcome (the standing rule now allows entry). Classified as snare because the extraction mechanism (dependence on proxy choice) persists even after entry is granted.
constraint_indexing:constraint_classification(remedies_article_32__pil_epistolary_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PROXY ADVOCATE (ROPE) — Benefits from standing relaxation: can file petitions on behalf of unnamed victims, can coordinate mass cases, can make test claims on constitutional questions without a specific paying client. The constraint creates a coordination function for the advocate: it converts individual suffering (diffuse, unorganized, voiceless) into actionable remedies (organized, litigable, visible). The advocate is a net beneficiary with arbitrage exit — if the cause loses interest, they move to another constituency. Low suppression: advocates have agency and choice. Classified as rope because the coordination function is genuine and the extraction is minimal.
constraint_indexing:constraint_classification(remedies_article_32__pil_epistolary_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE SOCIAL MOVEMENT ECOSYSTEM (TANGLED ROPE) — Standing relaxation enables mass mobilization: forest protection movements, bus safety campaigns, environmental litigation. But the constraint also embeds asymmetric extraction: the movement's claims are refracted through the court's procedural logic; outcomes are reinterpreted as judicial decrees rather than political victories; the movement's autonomy is constrained by the need to maintain standing (stay within constitutional bounds on remedy scope, avoid overreach that triggers backlash). Beneficiaries: the unrepresented constituencies the movement serves. Victims: the movement's own political autonomy and self-determination (it becomes an arm of judicial administration). Requires active enforcement of the standing gate to maintain the constraint's distinction from pure advocacy.
constraint_indexing:constraint_classification(remedies_article_32__pil_epistolary_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE WRONGED BUT UNORGANIZED CONSTITUENCY (SNARE) — Benefits from standing relaxation because they can now access courts without organizing themselves or hiring counsel. But they experience extraction through the proxy mechanism: their claims are filtered through the advocate's judgment about what is litigable, what is constitutionally sound, what will win. They do not control the remedy; the court and the advocate do. Suppression is high: constrained by lack of legal knowledge, literacy, resources, and access to communication networks. The constraint removes one gate (standing) but leaves all other gates intact. This perspective sees the constraint as beneficial but asymmetrically structured — the benefit comes with loss of autonomy over the claim.
constraint_indexing:constraint_classification(remedies_article_32__pil_epistolary_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ADVERSARIAL-FORM PURISM / THE VICTIM DOCTRINE (SNARE) — This is a 'perspective' from a doctrinal commitment rather than an agent, but it captures the real institutional constraint that standing relaxation violates. The classical adversarial form requires the plaintiff to be the party with direct interest in the remedy (the person who suffered the harm). Standing relaxation violates this: the proxy advocate may have no direct interest. The constraint extracts compliance from the adversarial doctrine by overriding it — the doctrine is the victim of the constraint. This perspective classifies the constraint as a snare because it suppresses the doctrine's core claims (no standing without direct harm; no remedies except for actual parties) and offers no principled exit (the doctrine cannot be held while standing is relaxed; it is overridden entirely). The suppression comes from the court's authority to rewrite the standing gate.
constraint_indexing:constraint_classification(remedies_article_32__pil_epistolary_reading, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: THE JUDICIAL SYSTEM (TANGLED ROPE) — Benefits from standing relaxation because it expands the court's reach, legitimacy, and social relevance. The constraint converts the court from a dispute-resolution mechanism into a social accountability forum — increasing its power and visibility. But the constraint also extracts from the court: it creates new administrative burdens (managing mass petitions, monitoring compliance with decrees), politicizes the judiciary (making courts visible advocates for the poor), and destabilizes the classical judicial role. The court experiences both genuine coordination (efficient aggregate remedy delivery) and asymmetric extraction (expansion into non-traditional judicial functions, politicization, compliance monitoring burdens). Requires active enforcement: the court must continuously redefine standing, manage the petition stream, and defend its jurisdictional reach against critics of judicial overreach.
constraint_indexing:constraint_classification(remedies_article_32__pil_epistolary_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: THE ANALYTICAL OBSERVER / CONSTITUTIONAL DEMOCRACY VIEW (SNARE) — From the civilizational/universal perspective, constitutionalism includes the right to remedy (access to justice). Standing relaxation is not a violation of constitutionalism — it is an instantiation of it. The constraint enables the constitutional promise (equal protection, fundamental rights) to reach those excluded by the classical adversarial form. Suppression from this view: the classical adversarial doctrine suppresses access to remedy for the voiceless. The constraint removes that suppression. However, the constraint also creates extraction: the democratized remedies are now filtered through judicial interpretation, not popular will. The constraint extracts from democracy (subordinates political voice to judicial wisdom). Classified as snare because the constraint privileges courts over popular movements as arbiters of rights — even though the outcome is more access to justice.
constraint_indexing:constraint_classification(remedies_article_32__pil_epistolary_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remedies_article_32__pil_epistolary_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(remedies_article_32__pil_epistolary_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(remedies_article_32__pil_epistolary_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(remedies_article_32__pil_epistolary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38, rising over interval): The constraint begins as a genuine access-to-remedy mechanism (ε ≈ 0.18 at t0) but extraction increases as the proxy advocate gatekeeping and judicial administration accumulate. At t0, the constraint's primary function is removing the standing barrier — pure coordination gain. By t30, the constraint has become a vehicle for proxy filtering and judicial policy-making, extracting autonomy from the constituencies it serves. The measurement trajectory shows the constraint's transformation from Rope (low extraction, genuine coordination) toward Tangled Rope (mixed coordination and extraction). Suppression (0.42, declining over interval): Classical suppression (no standing without direct harm, no remedy without a plaintiff) declines as the epistolary reading takes hold — the standing gate is removed. But new suppression emerges: the unrepresented constituency is suppressed by the proxy's choice of which claims are litigable and constitutionally sound. Net suppression declines (from 0.65 to 0.42) because the removal of the standing gate has real access-enabling effects. Theater ratio (0.35, rising over interval): The epistolary form (accepting postcards) is not theater — it is a genuine structural change. But as PIL matures, theater increases: the postcard-to-petition journey becomes a media narrative, the court becomes a political actor, and outcomes are reinterpreted as 'historic victories' rather than routine remedies. The rising theater reflects the constraint's increasing role as a symbol and movement-building tool, not just an access mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a dramatic perspectival gap between the victim of the constraint and its analytical observer. The imprisoned victim sees the constraint as enabling access (beneficial) but also as creating extraction (via proxy dependence). The proxy advocate sees pure coordination — they gain capacity to file petitions and organize diffuse claims. The social movement sees mixed benefits (expanded standing) and losses (judicial administration of political autonomy). Classical adversarial doctrine sees only suppression — its core principle (direct harm → standing) is overridden. The analytical observer at the constitutional level sees the constraint as both enabling and extractive: it enables access to remedy (constitutional promise), but it also subordinates democratic voice to judicial wisdom. The gap reveals that the constraint is not transparently beneficial or extractive — it is genuinely tangled, mixing coordination (access) with extraction (proxy gatekeeping, judicial administration). This tangling is the diagnostic signature: the constraint cannot be flattened to a single type.
 *
 * DIRECTIONALITY LOGIC:
 *   The power atoms and exit options are crucial for understanding directionality in this constraint. The imprisoned victim has zero exit options (trapped), making them powerless in structural terms — they can only access remedy through the proxy's goodwill. The proxy advocate has maximum exit options (arbitrage): they can take or decline any case, move to a new constituency, focus on different rights. The institutional court is constrained by its need to maintain jurisdictional boundaries and manage the petition stream — constrained exit, not arbitrage. The social movement experiences constrained exit: it needs the court's legitimacy but is subordinated to judicial interpretation. These differentiations in exit options drive the perspectival gap: the same structural feature (standing relaxation) appears as a snare to the imprisoned victim (extraction through proxy dependence), a rope to the proxy advocate (low extraction, pure coordination), a tangled_rope to the movement (mixed benefits and losses), and a snare to classical adversarial doctrine (override, not negotiation). Directionality overrides: None needed. The beneficiary/victim structure and exit options are sufficient to derive d values that match the perspectival classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   The pil_epistolary_reading resolves mandatrophy by showing that the constraint simultaneously coordinates access to remedy AND extracts autonomy from the constituencies it serves. The mandatrophy question — 'Is this pure coordination or extraction pretending to be coordination?' — has a nuanced answer: it is both, for different agents. The proxy advocate experiences coordination (low extraction, high benefit). The imprisoned victim experiences extraction (high proxy dependence, low autonomy). The social movement experiences tangled coordination and extraction (benefits from access, suffers from judicial administration). The analytical observer experiences a constitutional trade-off: gaining access while losing democratic control. The constraint is not mislabeled by calling it tangled_rope — the misclassification would be to call it 'pure PIL success' (rope) or 'pure judicial overreach' (snare). The tangling is structural, not a measurement error.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proxy_fidelity_alignment,
    'Does the proxy advocate''s legal theory and remedy seeking align with the actual preferences and priorities of the unrepresented constituency, or does the advocate''s interpretation of what is constitutionally achievable override what the constituency actually wants?',
    'Post-victory qualitative research: interviews with constituency members about whether the outcome matched their expectations; comparison of petition claims to actual remedies obtained; analysis of cases where advocates declined to pursue claims the constituency wanted',
    'If high alignment: constraint facilitates genuine voice for the voiceless (coordinate function strengthened). If low alignment: constraint becomes extraction mechanism (proxy captures the voice; unrepresented constituency''s autonomy is the victim). Classification may shift from tangled_rope to snare if alignment is systematically low.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_fidelity_alignment, empirical, 'Whether proxy advocates'' legal strategies align with constituency preferences').

omega_variable(
    standing_relaxation_vs_overreach_boundary,
    'Is the boundary between legitimate standing relaxation (proxy advocates filing on behalf of unrepresented poor) and judicial overreach (courts rewriting administrative policy, managing forests, directing budget allocation) a principled constitutional distinction, or is it an arbitrary line drawn by courts to maximize their own reach?',
    'Doctrinal analysis of standing case law; comparison of remedies granted in PIL cases to those available in traditional adversarial cases; empirical study of whether courts expand standing criteria when they believe the underlying cause (e.g., environmental protection) is important and contract standing when they believe it is not',
    'If principled distinction exists: PIL reading is a coherent doctrinal evolution. If arbitrary: PIL reading naturalizes judicial power-expansion; the writ_arsenal_reading''s concern about parallel government by decree becomes structurally grounded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(standing_relaxation_vs_overreach_boundary, conceptual, 'Whether standing relaxation has a principled boundary or enables unlimited judicial overreach').

omega_variable(
    epistolary_jurisdiction_necessity,
    'Is the epistolary jurisdiction (accepting petitions in letter form, from imprisoned or isolated victims) a structural necessity for reaching the voiceless, or a performative gesture that masks the constraint''s real gating mechanism (the advocate''s decision to take the case)?',
    'Historical and comparative analysis: do cases proceed faster/win more often when they arrive by postcard vs traditional pleadings? What proportion of epistolary petitions are actually accepted vs rejected? Do petitions rejected in letter form succeed if re-filed through counsel?',
    'If necessity: epistolary form is a genuine gate-removal that enables access (rope component of tangled_rope is real). If performative: the epistolary form is theater; the real gating mechanism is the advocate''s gatekeeping (snare component is dominant).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistolary_jurisdiction_necessity, empirical, 'Whether accepting epistolary petitions is structurally necessary or performative').

omega_variable(
    reading_contest_foreclosure,
    'Does the pil_epistolary_reading logically foreclose the pil_overreach_critique_reading, or do both readings coexist as live positions in contemporary constitutional discourse?',
    'Doctrinal analysis: can a framework simultaneously hold both (a) standing-relaxation is legitimate because it serves constitutional purposes (equal protection, access to remedy) AND (b) the remedies granted via standing relaxation exceed the court''s authority and create a parallel government? Or does accepting one require rejecting the other?',
    'If forecloses: PIL advocates must accept the overreach concern or abandon the epistolary reading. If coexist: both positions remain live; the constraint is contested but not logically contradictory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Logical relationship between epistolary reading and overreach critique').

omega_variable(
    kernel_reading_selection_authority,
    'What grounds the courts'' authority to relax standing via this particular reading of Article 32 rather than maintaining the writ_arsenal_reading (prerogative writs as formal machinery without relaxation) or the pil_overreach_critique_reading (standing as a principled gate against judicial overreach)?',
    'Analysis of the foundational court judgments that established PIL standing: what arguments did the court offer for why Article 32 permits standing relaxation? What alternative readings did the court consider and reject? Has the court''s grounding for the reading shifted over time (authority_erosion, practice_drift)?',
    'If grounding is deontological (rights-based): the reading is robust to empirical challenges. If grounding is instrumental (efficacy-based): the reading is vulnerable if PIL remedies fail to achieve stated goals. If grounding is conventional (institutional practice-based): the reading is vulnerable to counter-practice (courts reasserting classical standing requirements).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_authority, conceptual, 'What authority grounds the PIL epistolary reading of Article 32').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remedies_article_32__pil_epistolary_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pil_epis_theater_t0, remedies_article_32__pil_epistolary_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(pil_epis_theater_t15, remedies_article_32__pil_epistolary_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(pil_epis_theater_t30, remedies_article_32__pil_epistolary_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(pil_epis_extract_t0, remedies_article_32__pil_epistolary_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(pil_epis_extract_t15, remedies_article_32__pil_epistolary_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(pil_epis_extract_t30, remedies_article_32__pil_epistolary_reading, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(pil_epis_suppress_t0, remedies_article_32__pil_epistolary_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(pil_epis_suppress_t15, remedies_article_32__pil_epistolary_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(pil_epis_suppress_t30, remedies_article_32__pil_epistolary_reading, suppression_requirement, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remedies_article_32__pil_epistolary_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(remedies_article_32__pil_epistolary_reading, pil_overreach_critique_reading).
narrative_ontology:affects_constraint(remedies_article_32__pil_epistolary_reading, writ_arsenal_reading).
narrative_ontology:affects_constraint(remedies_article_32__pil_epistolary_reading, justiciability_boundary).

% DUAL FORMULATION NOTE:
% The pil_epistolary_reading is one of three constraint stories decomposing the contested kernel Article 32. The other readings (pil_overreach_critique_reading, writ_arsenal_reading) produce different constraint structures from the same text because they rest on different foundational principles and different interpretations of Article 32's scope. The network links show how the readings influence each other: the epistolary reading's relaxed standing creates conditions that the overreach reading identifies as problematic; the writ_arsenal reading provides a formal alternative grounding that neither relaxes standing nor claims overreach. Each story has its own ε, its own beneficiary/victim structure, and its own classification. Together, they model the contested terrain of Article 32 jurisprudence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
