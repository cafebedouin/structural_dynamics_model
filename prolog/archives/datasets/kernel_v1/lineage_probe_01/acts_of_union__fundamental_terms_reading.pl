% ============================================================================
% CONSTRAINT STORY: acts_of_union__fundamental_terms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acts_of_union__fundamental_terms_reading, []).

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
 *   constraint_id: acts_of_union__fundamental_terms_reading
 *   human_readable: Acts of Union: Fundamental Terms Reading (Entrenchment Doctrine)
 *   domain: constitutional_law/scottish_legal_doctrine
 *
 * SUMMARY:
 *   The Acts of Union 1707 secured the Kirk of Scotland and Scots law 'in all
 *   time coming' — language that the Scottish nationalist legal scholar Neil
 *   MacCormick read as constitutional entrenchment, binding even the
 *   sovereign Westminster Parliament. This reading claims that certain Union
 *   articles are beyond repeal, not because they are higher law (the UK has
 *   no written constitution) but because repealing them would dissolve the
 *   Union itself, making the repeal legally incoherent. This constraint story
 *   instantiates ONE reading of a deeply contested kernel: the Acts of Union
 *   as a foundational text grounded in lineage authority (the 1707
 *   settlement) that different parties read fundamentally differently. The
 *   fundamental-terms reading positions entrenchment as real and operative,
 *   suppressing Westminster's universal sovereignty. The incorporating-union
 *   reading treats the Union as a unitary state where Scottish institutions
 *   survived as exceptions but remain revocable. The ordinary-statute reading
 *   (the orthodox doctrinal position) holds that Union Acts are statutes like
 *   any other, subject to amendment by Parliament. This story generates the
 *   fundamental-terms reading as a clean ε-invariant constraint with its own
 *   beneficiaries (Kirk, Scottish law), victims (unlimited-sovereignty
 *   doctrine, codified constitutionalism), and classified type. It does NOT
 *   describe the contest between readings — that contest is recorded in omega
 *   variables and cs_structure. The extractiveness trajectory (0.18 → 0.38
 *   over 150 years) reflects the entrenchment doctrine's accumulating
 *   rhetorical force in Scottish political discourse, especially
 *   post-devolution, even as its formal legal status remains contested and
 *   suppressed by orthodoxy. The theater ratio's rise (0.35 → 0.58) indicates
 *   that the entrenchment claim has become increasingly performative —
 *   invoked in Scottish nationalist argument more for its symbolic power than
 *   for its operational legal force.
 *
 * KEY AGENTS:
 *   - Scottish Kirk: Primary beneficiary (institutional/arbitrage) — entrenchment doctrine protects religious settlement 'in all time coming' against Westminster override
 *   - Scottish Legal System: Secondary beneficiary (institutional/arbitrage) — Scots law guaranteed by Article XIX beyond repeal
 *   - Westminster Parliament: Constrained actor (institutional/constrained) — on this reading, cannot override fundamental terms despite parliamentary sovereignty doctrine
 *   - Scottish Devolved Parliament: Moderate actor (moderate/constrained) — benefits from entrenchment protection but constrained by its ambiguous scope
 *   - Unlimited-Sovereignty Doctrine: Victim (analytical/trapped) — entrenchment reading suppresses the universality of parliamentary sovereignty
 *   - Codified Constitutionalist: Victim (powerless/trapped) — forced to choose between fragmented sovereignty (some articles unamendable, others not) or rejecting entrenchment entirely
 *   - Ordinary-Statute Doctrinal Tradition: Institutional actor (institutional/constrained) — constrained by the persistent MacCormickian heresy in Scottish discourse despite its incompatibility with orthodox law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acts_of_union__fundamental_terms_reading, 0.38).
domain_priors:suppression_score(acts_of_union__fundamental_terms_reading, 0.62).
domain_priors:theater_ratio(acts_of_union__fundamental_terms_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acts_of_union__fundamental_terms_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(acts_of_union__fundamental_terms_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(acts_of_union__fundamental_terms_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acts_of_union__fundamental_terms_reading, tangled_rope).
narrative_ontology:human_readable(acts_of_union__fundamental_terms_reading, "Acts of Union: Fundamental Terms Reading (Entrenchment Doctrine)").
narrative_ontology:topic_domain(acts_of_union__fundamental_terms_reading, "constitutional_law/scottish_legal_doctrine").

domain_priors:requires_active_enforcement(acts_of_union__fundamental_terms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acts_of_union__fundamental_terms_reading, '3733f6ab-39e8-4f12-b566-e82d05c3c7c3').
narrative_ontology:cs_kernel_codification('3733f6ab-39e8-4f12-b566-e82d05c3c7c3', formalized).
narrative_ontology:cs_authority_grounding('3733f6ab-39e8-4f12-b566-e82d05c3c7c3', lineage).
narrative_ontology:cs_interpretation_layer_present('3733f6ab-39e8-4f12-b566-e82d05c3c7c3').
narrative_ontology:cs_reading_relation('3733f6ab-39e8-4f12-b566-e82d05c3c7c3', acts_of_union__incorporating_union_reading, coexists_with).
narrative_ontology:cs_reading_relation('3733f6ab-39e8-4f12-b566-e82d05c3c7c3', acts_of_union__ordinary_statute_reading, forecloses).
narrative_ontology:cs_axiom('3733f6ab-39e8-4f12-b566-e82d05c3c7c3', foundational, union_articles_unamendable_in_perpetuity).
narrative_ontology:cs_axiom_status(union_articles_unamendable_in_perpetuity, holdable).
narrative_ontology:cs_axiom_grounding('3733f6ab-39e8-4f12-b566-e82d05c3c7c3', union_articles_unamendable_in_perpetuity, conventional).
narrative_ontology:cs_axiom('3733f6ab-39e8-4f12-b566-e82d05c3c7c3', secondary, union_dissolution_consequence_of_article_repeal).
narrative_ontology:cs_axiom_status(union_dissolution_consequence_of_article_repeal, holdable).
narrative_ontology:cs_axiom_grounding('3733f6ab-39e8-4f12-b566-e82d05c3c7c3', union_dissolution_consequence_of_article_repeal, deontological).
narrative_ontology:cs_reference_frame('3733f6ab-39e8-4f12-b566-e82d05c3c7c3', lineage_covenant_union).
narrative_ontology:cs_drift_state('3733f6ab-39e8-4f12-b566-e82d05c3c7c3', contemporary_westminster_sovereignty_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3733f6ab-39e8-4f12-b566-e82d05c3c7c3', '').
narrative_ontology:cs_kernel_id(acts_of_union__fundamental_terms_reading, acts_of_union).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acts_of_union__fundamental_terms_reading, scottish_kirk).
narrative_ontology:constraint_beneficiary(acts_of_union__fundamental_terms_reading, scottish_legal_system).
narrative_ontology:constraint_victim(acts_of_union__fundamental_terms_reading, unlimited_westminster_sovereignty).
narrative_ontology:constraint_victim(acts_of_union__fundamental_terms_reading, codified_british_constitutionalism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CODIFIED CONSTITUTIONALIST (SNARE) — Trapped by the entrenchment doctrine's denial of Westminster's plenary legislative power. Cannot exit the framework without abandoning constitutional coherence. Bears the cost of a fragmented sovereignty model where some articles are allegedly unamendable while others remain subject to ordinary amendment. Maximum extraction from the standpoint of unified legal order.
constraint_indexing:constraint_classification(acts_of_union__fundamental_terms_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SCOTTISH KIRK (ROPE) — Primary beneficiary. The entrenchment doctrine protects the Kirk's position as guaranteed 'in all time coming' — no Westminster Parliament can repeal Article I's religious settlement without breach of the Union itself. Benefits from coordination without bearing suppression costs. Can arbitrage the doctrine to resist English ecclesiastical authority.
constraint_indexing:constraint_classification(acts_of_union__fundamental_terms_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: SCOTTISH LEGAL SYSTEM (ROPE) — Secondary beneficiary. Article XIX guarantees Scots law 'in all time coming' — the entrenchment reading secures this beyond Westminster override. Experiences the constraint as pure coordination: a guarantee against absorption into English law. Net beneficiary with institutional exit capacity.
constraint_indexing:constraint_classification(acts_of_union__fundamental_terms_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SCOTTISH DEVOLVED PARLIAMENT (TANGLED ROPE) — Constrained by the entrenchment doctrine's ambiguity: does it apply to devolved competence or only to reserved matters? The doctrine provides some protection against Westminster override but also constrains flexibility in updating Scotland's governance. Mixed coordination (guaranteed legislative space) and extraction (limits on amendment procedures).
constraint_indexing:constraint_classification(acts_of_union__fundamental_terms_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: WESTMINSTER PARLIAMENT (TANGLED ROPE) — Constrained by the entrenchment doctrine on one reading, unlimited on the orthodox reading. If entrenchment holds, Westminster has genuine coordination obligations (respecting Scottish institutions) but also pays extraction costs (cannot unilaterally override the Union terms). Coordination function: maintaining the Union's structure. Extraction: inability to govern all-UK matters uniformly.
constraint_indexing:constraint_classification(acts_of_union__fundamental_terms_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ORDINARY STATUTE DOCTRINE (PITON) — From the perspective of orthodox constitutional law, the entrenchment reading is a degraded vestige: MacCormick's heresy invokes unamendable articles that have been functionally superseded by subsequent Acts (Union with Ireland 1801, devolution settlements 1998). The doctrine persists through historical inertia and nationalist sentiment rather than constitutional function. Theater ratio high because the entrenchment claim maintains performative force in Scottish legal discourse despite being incompatible with British constitutional doctrine.
constraint_indexing:constraint_classification(acts_of_union__fundamental_terms_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: LEGAL POSITIVIST / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, sovereignty cannot be entrenced: a sovereign lawmaker cannot bind its successor. This is presented as an immutable principle of law itself — Blackstone's doctrine that Parliament cannot limit Parliament's power is treated as a natural law of legislative systems. However, the structural data contradicts this: the entrenchment doctrine is not a natural limit but a contested reading of a specific historical document. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(acts_of_union__fundamental_terms_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acts_of_union__fundamental_terms_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(acts_of_union__fundamental_terms_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(acts_of_union__fundamental_terms_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(acts_of_union__fundamental_terms_reading, TR),
    TR >= 0.70.

:- end_tests(acts_of_union__fundamental_terms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The entrenchment doctrine extracts from the standpoint of Westminster's unlimited sovereignty and from codified constitutionalism's coherence requirement. But the extraction is not severe because: (a) the doctrine has never been definitively enforced against Westminster (remains more threat than practice), (b) Westminster has never actually attempted the repeal the doctrine forbids (nuclear option remains unused), and (c) Scottish institutions themselves have not consistently invoked entrenchment as their primary defense. The extractiveness represents the suppression of alternatives and the accumulated rhetorical force of the doctrine in Scottish discourse rather than active, operational extraction. Suppression (0.62): Moderate-high. The entrenchment doctrine is suppressed by official constitutional doctrine (parliamentary sovereignty), by Westminster's practical ability to override it, and by the absence of a court willing to enforce it. Yet suppression is incomplete — the doctrine survives in Scottish legal education, nationalist politics, and academic jurisprudence. It functions as a suppressed-but-not-dead alternative. Theater ratio (0.58): Moderate-high and rising. The entrenchment claim is increasingly performative — invoked to assert Scottish distinctiveness rather than to block specific Westminster action. No enforcement mechanism exists. The doctrine's force derives from its invocation in political discourse rather than from legal operationalization. The trajectory from 0.35 to 0.58 reflects the shift from a dormant doctrinal claim (1707–1950s) to an active symbol in Scottish nationalist argument (1950s onward, accelerating post-1979 devolution). Claimed type: Tangled Rope. The constraint exhibits both genuine coordination (protecting Scottish institutions against absorption) and asymmetric extraction (suppression of Westminster's universality, benefiting the Kirk/Scottish law at the expense of unified sovereignty). The requirement for active enforcement reflects that maintaining entrenchment as operative doctrine requires sustained Scottish institutional assertion and Westminster political forbearance — it is not self-executing.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence from a single constraint. The Scottish Kirk sees pure coordination (Rope) — the doctrine guarantees their religious autonomy. The Scottish legal system sees rope — guaranteed jurisdiction over Scots law. The Scottish Devolved Parliament sees mixed coordination and extraction (Tangled Rope) — protection and constraint intertwined. Westminster sees its own power limited (Tangled Rope) — genuine obligation to respect the Union terms but also inability to govern uniformly. The codified constitutionalist sees pure extraction (Snare) — forced to deny either entrenchment (abandoning the reading) or sovereignty (accepting fragmentation). The ordinary-statute doctrine sees entrenchment as a vestigial and performative claim (Piton) — invoked but not functional, maintained by political tradition rather than legal force. The universal legal positivist sees entrenchment as logically impossible (Mountain) — sovereigntry cannot be bound — but this perspective naturalizes a contested doctrine rather than describing a natural law. The widest gap is between the beneficiaries (Kirk, Scots law) who experience coordination and the doctrinal traditions (ordinary statute, legal positivism) that experience extraction or impossibility. This gap reveals the reading's fundamental claim: that the doctrine redistributes power by suppressing Westminster's universality.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from structural position: beneficiaries with institutional power and arbitrage exit (Kirk, Scottish law) get low d (beneficiary position → f(d) ≈ 0.02, negative effective extraction). The unlimited-sovereignty doctrine and codified-constitutionalist perspectives are victims of the suppression — they experience maximum extraction when the entrenchment reading is operative (high d → high f(d)). Westminster Parliament occupies a mixed position: beneficiary of the coordinate status quo (d ≈ 0.50) but victim of the limitation on override power. The Scottish Devolved Parliament is constrained by the doctrine's ambiguity (d ≈ 0.55, moderate extraction from the uncertainty). The canonical derivation chain computes these without override except where noted: for Westminster's dual status as both beneficiary (coordinate Union) and victim (limited override), the perspective is marked constrained not arbitrage to reflect the constraint's asymmetry.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    entrenchment_doctrine_status,
    'Does the ''in all time coming'' language in Articles I and XIX establish genuine constitutional entrenchment, or is it merely a statement of intent without binding legal force on successor Parliaments?',
    'Case law trajectory: MacCormick v Lord Advocate (1953) established grounds for review but left entrenchment question open. Resolution requires judicial confirmation that Westminster cannot override specified articles. Alternatively, resolution may come from Westminster''s own refusal to override these articles, establishing political convention that substitutes for legal entrenchment.',
    'If entrenchment is real: constraint is Tangled Rope/Rope (genuine coordination with limited extraction). Classification stabilizes at fundamental-terms-reading''s intentional type. If entrenchment is theater: constraint degrades to Piton (performative claim in Scottish discourse without legal force). Classification shifts toward ordinary-statute-reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenchment_doctrine_status, empirical, 'Whether Union articles establish genuine entrenchment or performative claim').

omega_variable(
    sovereignty_paradox_resolution,
    'Can the doctrine of parliamentary sovereignty coexist with entrenchment of specific articles, or does entrenchment logically foreclose unlimited sovereignty?',
    'Jurisprudential analysis: compare entrenchment doctrine with post-Brexit reconstructions of parliamentary sovereignty (especially R v Gina Miller litigation). If new sovereigntist theory permits entrenchment as a voluntary limitation Parliament can adopt and revoke, entrenchment and sovereignty coexist. If sovereigntist doctrine demands absolute veto power, entrenchment is foreclosed.',
    'If coexistence possible: reading coexists_with ordinary-statute-reading (both frameworks can hold simultaneously in different institutions). If foreclosure occurs: fundamental-terms-reading forecloses ordinary-statute-reading within a single sovereignty framework, or vice versa.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_paradox_resolution, conceptual, 'Whether entrenchment and parliamentary sovereignty can coexist logically').

omega_variable(
    beneficiary_institution_authenticity,
    'Do the Scottish Kirk and Scottish legal system genuinely benefit from entrenchment protection, or has the entrenchment doctrine become detached from the institutions it claims to protect?',
    'Institutional analysis: (a) Do the Kirk and Scottish courts invoke the entrenchment doctrine to resist Westminster authority? (b) Have these institutions themselves endorsed or disavowed MacCormick''s heresy? (c) Has Westminster ever actually threatened to override Articles I or XIX, requiring entrenchment as defensive shield?',
    'If institutions actively use entrenchment: beneficiary classification is structurally accurate; extraction from codified-constitutionalist perspective is real and ongoing. If institutions remain silent or hostile: entrenchment doctrine may be an academic construct detached from institutional interests, altering the beneficiary/victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_institution_authenticity, empirical, 'Whether protected Scottish institutions genuinely benefit from entrenchment doctrine').

omega_variable(
    reading_coexistence_framework,
    'Can this reading coexist with the ordinary-statute and incorporating-union readings within a single constitutional framework, or do the readings inhabit mutually exclusive interpretive traditions?',
    'Doctrinal mapping: trace which readings are held by Scottish courts, English courts, Westminster actors, and Scottish nationalists respectively. Assess whether any single institutional actor holds multiple readings simultaneously, or whether readings are cleanly partitioned across institutional jurisdictions and political camps.',
    'If genuinely coexisting: omega marks the constraint as fundamentally contested, with no adjudicating authority (distributed authority_grounding). If partitioned by jurisdiction: readings may coexist institutionally but foreclose or influence each other doctrinally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coexistence_framework, conceptual, 'Whether the three Union readings can coexist in a single framework').

omega_variable(
    asymmetry_maintenance_mechanism,
    'How does the entrenchment doctrine maintain suppression (0.62) of alternative framings? What institutional or rhetorical mechanisms prevent the ordinary-statute reading from definitively displacing the entrenchment claim?',
    'Discourse analysis: identify legal, political, and nationalist arguments that sustain entrenchment despite official doctrine (parliamentary sovereignty) denying it. Mechanisms may include: (a) Scottish legal exceptionalism tradition, (b) nationalist political pressure, (c) judicial reluctance to definitively foreclose, (d) Westminster political avoidance of explicit override.',
    'Understanding suppression mechanisms clarifies whether the constraint operates through formal legal prohibition (high suppression, low theater) or through political/rhetorical foreclosure (lower suppression, higher theater). Affects theater_ratio calibration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetry_maintenance_mechanism, empirical, 'How entrenchment doctrine maintains suppression against competing readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acts_of_union__fundamental_terms_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(union_fund_tr_t0, acts_of_union__fundamental_terms_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(union_fund_tr_t100, acts_of_union__fundamental_terms_reading, theater_ratio, 100, 0.5).
narrative_ontology:measurement(union_fund_tr_t150, acts_of_union__fundamental_terms_reading, theater_ratio, 150, 0.58).

% Extraction over time
narrative_ontology:measurement(union_fund_be_t0, acts_of_union__fundamental_terms_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(union_fund_be_t100, acts_of_union__fundamental_terms_reading, base_extractiveness, 100, 0.28).
narrative_ontology:measurement(union_fund_be_t150, acts_of_union__fundamental_terms_reading, base_extractiveness, 150, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(union_fund_su_t0, acts_of_union__fundamental_terms_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(union_fund_su_t100, acts_of_union__fundamental_terms_reading, suppression_requirement, 100, 0.58).
narrative_ontology:measurement(union_fund_su_t150, acts_of_union__fundamental_terms_reading, suppression_requirement, 150, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acts_of_union__fundamental_terms_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(acts_of_union__fundamental_terms_reading, acts_of_union__incorporating_union_reading).
narrative_ontology:affects_constraint(acts_of_union__fundamental_terms_reading, acts_of_union__ordinary_statute_reading).
narrative_ontology:affects_constraint(acts_of_union__fundamental_terms_reading, scottish_sovereignty_doctrine).
narrative_ontology:affects_constraint(acts_of_union__fundamental_terms_reading, devolution_settlement_scope).

% DUAL FORMULATION NOTE:
% The Acts of Union kernel generates three structurally distinct constraints corresponding to three readings: (1) fundamental_terms_reading (this file) — ε=0.38, Tangled Rope, claims entrenchment of Kirk and Scots law 'in all time coming'; (2) incorporating_union_reading — ε≈0.25, Rope, claims Scottish institutions protected as exceptions within unitary state but revocable; (3) ordinary_statute_reading — ε≈0.08, Rope, claims Union Acts are ordinary statutes subject to amendment by Parliament. Each reading has different beneficiaries (fundamental_terms: Kirk/Scots law; incorporating_union: Scottish institutional autonomy; ordinary_statute: Westminster legislative universality), different suppression profiles, and different theater ratios. They are not observables of a single constraint — they are three distinct constraints rooted in a single contested kernel. All three stories should link via network.affects_constraints to show the kernel family. The fundamental_terms_reading is the most extractive of the three (0.38) because it suppresses Westminster's universality; the ordinary_statute_reading is the least extractive (0.08) because it denies suppression entirely; the incorporating_union_reading is intermediate (0.25) because it acknowledges Scottish exception but treats it as revocable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acts_of_union__fundamental_terms_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
