% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__hierarchical_indissolubility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__hierarchical_indissolubility_reading, []).

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
 *   constraint_id: marriage_sacrament__hierarchical_indissolubility_reading
 *   human_readable: Marriage as Sacramental Ontology: Hierarchical Indissolubility Reading
 *   domain: religious_doctrine/canon_law/political_sociology
 *
 * SUMMARY:
 *   The hierarchical indissolubility reading grounds the Catholic Church's
 *   doctrine of marriage in a metaphysical claim: marriage is a permanent,
 *   indissoluble sacrament constitutively bound by God's creative will, not
 *   subject to dissolution by human action or institutional permission. This
 *   reading treats marriage ontologically — what God has joined cannot be put
 *   asunder — and adjudicates the meaning of marriage through a hierarchical
 *   magisterium whose authority is grounded in apostolic succession and papal
 *   primacy. Divorce is not permitted; remarriage while the first spouse
 *   lives is treated as 'adultery.' The constraint operates through
 *   sacramental denial: divorced-and-remarried Catholics are excluded from
 *   Eucharistic communion and other sacraments unless they obtain an
 *   annulment (a tribunal judgment that the marriage was never sacramentally
 *   valid) or commit to permanent celibacy. The constraint's extractiveness
 *   lies in the institutional mechanism: annulment processes are controlled
 *   by the Church hierarchy, extremely costly (in time and money), subject to
 *   variable approval rates by diocese, and functionally serve as gatekeeping
 *   of remarriage permission. The suppression is high and intensifying:
 *   pastoral communities have minimal authority to exercise mercy; divorced
 *   Catholics face identity-lock (remaining Catholic means accepting
 *   exclusion) or exit (leaving the Church or moving to a more lenient
 *   tradition). The theater ratio is moderate and rising: post-Vatican II
 *   reforms (1983 Code, 2015 Francis reforms) have increased the appearance
 *   of pastoral responsiveness while the underlying extraction mechanism
 *   persists.
 *
 * KEY AGENTS:
 *   - Divorced Catholics seeking remarriage: Primary victims (powerless/identity_locked) — experience ontological exclusion from the Eucharist; identity is fused with Catholic identity but constraint requires either permanent celibacy or exit.
 *   - Pastoral communities (priests, parishes): Secondary victims (moderate/constrained) — see pastorally appropriate paths but face hierarchical discipline and loss of standing for defection; authority is suppressed.
 *   - Hierarchical magisterium (Pope, bishops, Vatican Curia): Primary beneficiary (institutional/constrained) — maintains centralized authority over doctrinal interpretation and sacramental discipline; controls annulment gatekeeping; consolidates institutional power.
 *   - Tribunal system (diocesan marriage courts): Institutional beneficiary (institutional/arbitrage) — operates as slow revenue stream; functions as status affirmation for hierarchical authority; maintains appearance of rigorous investigation.
 *   - Doctrinal tradition (Thomistic natural law): Ideational beneficiary (powerful/mobile) — indissolubility teaching is core to Church's identity and institutional continuity; maintains magisterium's claim to teach unchanging truth.
 *   - Analytical observer: Disinterested evaluator (analytical/analytical) — sees the constraint as potentially falsely natural; metaphysical claims mask institutional extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, 0.58).
domain_priors:suppression_score(marriage_sacrament__hierarchical_indissolubility_reading, 0.72).
domain_priors:theater_ratio(marriage_sacrament__hierarchical_indissolubility_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__hierarchical_indissolubility_reading, snare).
narrative_ontology:human_readable(marriage_sacrament__hierarchical_indissolubility_reading, "Marriage as Sacramental Ontology: Hierarchical Indissolubility Reading").
narrative_ontology:topic_domain(marriage_sacrament__hierarchical_indissolubility_reading, "religious_doctrine/canon_law/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__hierarchical_indissolubility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__hierarchical_indissolubility_reading, '3d2fec36-a5aa-4f8f-b137-154744fe5801').
narrative_ontology:cs_kernel_codification('3d2fec36-a5aa-4f8f-b137-154744fe5801', formalized).
narrative_ontology:cs_authority_grounding('3d2fec36-a5aa-4f8f-b137-154744fe5801', extraction).
narrative_ontology:cs_interpretation_layer_present('3d2fec36-a5aa-4f8f-b137-154744fe5801').
narrative_ontology:cs_reading_relation('3d2fec36-a5aa-4f8f-b137-154744fe5801', marriage_sacrament__civic_pastoral_reading, coexists_with).
narrative_ontology:cs_axiom('3d2fec36-a5aa-4f8f-b137-154744fe5801', foundational, marriage_ontologically_indissoluble).
narrative_ontology:cs_axiom_status(marriage_ontologically_indissoluble, holdable).
narrative_ontology:cs_axiom_grounding('3d2fec36-a5aa-4f8f-b137-154744fe5801', marriage_ontologically_indissoluble, theological).
narrative_ontology:cs_axiom('3d2fec36-a5aa-4f8f-b137-154744fe5801', foundational, hierarchical_magisterium_final_arbiter).
narrative_ontology:cs_axiom_status(hierarchical_magisterium_final_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('3d2fec36-a5aa-4f8f-b137-154744fe5801', hierarchical_magisterium_final_arbiter, conventional).
narrative_ontology:cs_reference_frame('3d2fec36-a5aa-4f8f-b137-154744fe5801', thomistic_natural_law_sacramental_theology).
narrative_ontology:cs_drift_state('3d2fec36-a5aa-4f8f-b137-154744fe5801', post_vatican_ii_era_to_francis_papacy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3d2fec36-a5aa-4f8f-b137-154744fe5801', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, hierarchical_magisterium).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, institutional_authority_structure).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, divorced_catholics_seeking_remarriage).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, pastoral_communities_serving_separated_persons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIVORCED CATHOLIC (SNARE) — Identity-locked within Catholic worldview; cannot participate in Eucharist without annulment (prohibitively expensive, years-long process, uncertain outcome). Sacramental exclusion is experienced as ontological death — the constraint's force is not economic penalty but constitutive denial of access to the community's central ritual. Structurally mobile (can join another Christian tradition or leave), but identity is fused with Catholic identity; exit means becoming someone else. Maximum experienced extraction: full sacramental exclusion for those the reading classifies as living in 'adultery.'
constraint_indexing:constraint_classification(marriage_sacrament__hierarchical_indissolubility_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: PASTORAL COMMUNITY (SNARE) — Priests and parish communities serving divorced-and-remarried Catholics face the constraint as institutional suppression: they see pastorally appropriate paths (discernment, remarriage blessing, expanded Eucharistic inclusion) but cannot implement them without facing hierarchical discipline. High cost to defection (loss of standing, reassignment, defrocking). Extraction: pastoral authority is subordinated to hierarchical adjudication; local communities cannot exercise mercy without institutional permission. Suppression mechanism is jurisdictional — the parish has no formal authority over sacramental access decisions.
constraint_indexing:constraint_classification(marriage_sacrament__hierarchical_indissolubility_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HIERARCHICAL MAGISTERIUM (TANGLED ROPE) — The reading's native institutional perspective. Sees the constraint as a genuinely coordination function: indissolubility doctrine coordinates the Church's teaching on covenant fidelity, sacramental integrity, and institutional authority. BUT also extracts asymmetric benefit: enforcement of indissolubility centralizes matrimonial authority (only bishops/tribunals can adjudicate annulments), concentrates epistemic authority (only magisteri can interpret what 'true marriage' means), and sustains institutional control over pastoral practice. Genuine coordination (fidelity teaching) + asymmetric extraction (authority concentration). Active enforcement required — magisterium must suppress alternative pastoral readings.
constraint_indexing:constraint_classification(marriage_sacrament__hierarchical_indissolubility_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: DOCTRINAL TRADITION (ROPE) — From the civilizational view of Thomistic natural law, indissolubility appears as pure coordination: marriage as a natural institution (not invented by the Church) with intrinsic properties (permanence, fidelity, openness to procreation) that coordinate family stability and social continuity. This perspective sees no extraction — the reading is transparent about what the constraint is. But this perspective is also mobile (can be superseded by different readings of natural law); it does not perceive the institutional extraction mechanism because it remains at the level of doctrine rather than enforcement.
constraint_indexing:constraint_classification(marriage_sacrament__hierarchical_indissolubility_reading, rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TRIBUNAL SYSTEM (PITON) — The annulment tribunal process (established 1983 after John Paul II's reform) is largely performative: the canonical machinery moves slowly (5–10 year average), imposes substantial fees ($500–$3000 in US dioceses), and produces highly variable outcomes (98%+ approval rate in some dioceses, much lower in others). The tribunals perceive their own degradation — the system maintains the appearance of rigorous investigation while actually functioning as a slow revenue stream and status affirmation for hierarchical authority. Theater ratio reflects: the annulment process is substantively about confirming that the marriage was never 'real' (ontologically), but functionally about institutional gatekeeping of remarriage permission. Piton: maintained through institutional inertia, not because it works.
constraint_indexing:constraint_classification(marriage_sacrament__hierarchical_indissolubility_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / METAPHYSICAL VIEW (MOUNTAIN) — From a purely logical/metaphysical perspective, this reading treats indissolubility as an immutable property of sacramental reality: if marriage is constitutively a perpetual covenant (as the reading claims), then divorce cannot change that ontological fact — attempted divorce is like division by zero, a conceptual impossibility. From this view, the constraint is not enforced but discovered. However, this perspectival mountain is vulnerable to the false-summit diagnosis: the 'immutability' of sacramental marriage depends on a specific metaphysical claim (that marriage has an ontological essence grounded in God's creative will), not on logical necessity. Alternative metaphysical framings (marriage as a contractual relationship dissoluble by consent, or as an evolving covenant subject to pastoral reinterpretation) produce entirely different 'natural' constraints.
constraint_indexing:constraint_classification(marriage_sacrament__hierarchical_indissolubility_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marriage_sacrament__hierarchical_indissolubility_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_sacrament__hierarchical_indissolubility_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(marriage_sacrament__hierarchical_indissolubility_reading, TR),
    TR >= 0.70.

:- end_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over the interval. Initial extractiveness (t=1965, post-Vatican II): 0.35. The 1983 Code of Canon Law reformed annulment procedures, ostensibly to make them more pastoral, but functionally maintained the hierarchical gatekeeping and increased the appearance of accessibility without substantive change in approval logic. By 2005, extractiveness has risen to 0.58 as the gap between the reading's pastoral language and its institutional enforcement mechanism has become evident to sociologists and critics. The extraction is not primarily economic (annulment fees are income, but not the primary driver) but ontological and epistemic: the constraint extracts sacramental access (who gets to participate in Eucharist), definitional authority (who gets to say what counts as a 'real' marriage), and pastoral authority (only the hierarchy can grant exceptions to the doctrine). Suppression (0.72): High and rising. The suppression mechanism operates through sacramental denial, jurisdictional control, and identity-lock. Divorced Catholics face a binary: remain a 'bad' Catholic (excluded from Eucharist) or exit. Pastoral communities face hierarchical discipline for defection. The suppression is enforced not primarily through legal penalty but through the withholding of access to the community's central ritual and through internalized identity shame. Theater ratio (0.55): Moderate and rising. The annulment process is performative: it performs the appearance of rigorous investigation into whether the marriage was ever sacramentally valid, but functionally it is a slow-moving institutional gatekeeping mechanism. Post-Vatican II reforms (especially the simplified 1983 process and Francis's 2015 reforms allowing single-bishop confirmation) increased the theatrical appearance of accessibility while maintaining the underlying extraction mechanism. The theater has risen as the gap between pastoral language and institutional reality has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a severe perspectival gap between the beneficiary institutional perspective (tangled rope with genuine coordination function + asymmetric extraction) and the victim powerless perspective (snare with identity-lock). The magisterium sees a coherent doctrine coordinating fidelity, covenant integrity, and institutional authority; the divorced Catholic sees sacramental exclusion and identity denial. The tribunal system sees itself as administering canonical procedure; pastors see bureaucratic gatekeeping. The Scholastic natural law reading sees pure doctrine (rope); the institutional extraction mechanism renders it a snare. The analytical observer risks seeing the metaphysical claim as a mountain (marriage has an ontological essence) but the false-summit detector reveals this as naturalization of an institutional arrangement. The key perspectival disagreement: is the permanence of marriage a property of reality that the hierarchy merely administers, or is it an institutional claim the hierarchy enforces?
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality flows from its structure as institutional extraction masked by metaphysical language. The hierarchical magisterium (institutional/constrained) has low directionality (d ≈ 0.15–0.25) as a beneficiary with arbitrage options — it can maintain doctrinal authority without the constraint if needed, though it chooses to enforce it. Divorced Catholics (powerless/identity_locked) have high directionality (d ≈ 0.85–0.95) as victims with no effective exit: they cannot reject the doctrine without rejecting their Catholic identity. The reading's core premise (marriage is ontologically indissoluble) is itself a directional claim — it operates asymmetrically. It does not say 'marriage is usually permanent and we recommend you treat it that way' (symmetrical); it says 'marriage cannot be dissolved because God has bound it' (asymmetrical, with enforcement). The pastoral community (moderate/constrained) experiences intermediate directionality (d ≈ 0.55–0.65): they have some agency to defect (leave priesthood, move to reform movements) but face career and identity costs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_institutional_marriage,
    'Is the binding force of sacramental indissolubility ontological (marriage has an immutable metaphysical essence grounded in God''s creative act) or institutional (the Church enforces indissolubility doctrine through canonical authority and sacramental denial)?',
    'Theological analysis: does the reading ground indissolubility in the nature of the sacrament itself (ontological) or in the Church''s magisterial authority to interpret and enforce the sacrament (institutional)? Historical analysis: has the Church''s enforcement mechanism changed (Vatican II reforms, 1983 code changes, Pope Francis''s 2015 reforms) while claiming the doctrine is unchanging?',
    'If ontological: the constraint is a mountain (immutable property of reality); institutional enforcement is merely revealing what is already true. If institutional: the constraint is a snare (extraction mechanism masked by metaphysical language); the ''immutability'' is enforced, not discovered.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontological_vs_institutional_marriage, conceptual, 'Whether indissolubility is grounded in ontology or institutional authority').

omega_variable(
    annulment_as_true_investigation_vs_gatekeeping,
    'Does the annulment tribunal process constitute a genuine pastoral investigation into whether a marriage was ever sacramentally valid, or does it primarily function as an institutional gatekeeping mechanism for hierarchical control of remarriage?',
    'Comparative analysis: approval rates by diocese, correlation between tribunal rigor and local pastoral tradition. Case analysis: how many annulments are granted on grounds the initiating party (usually the petitioner) believes are theologically sound vs. grounds the tribunal constructs post-hoc? Temporal analysis: has the approval rate shifted with changes in canonical procedure (1983 reform, Francis''s 2015 reforms) despite no claimed shift in doctrine?',
    'If genuine investigation: the tribunal is (slow, expensive, but) attempting to apply doctrine faithfully; extraction is a side effect of procedural overhead, not the primary function. If primarily gatekeeping: the constraint is pure snare dressed in procedural legitimacy; the theater_ratio is the core phenomenon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(annulment_as_true_investigation_vs_gatekeeping, empirical, 'Whether tribunal process is genuine investigation or institutional gatekeeping').

omega_variable(
    magisterial_authority_grounding,
    'What grounds the hierarchical magisterium''s authority to make final adjudications on marriage validity? Is it divinely mandated infallibility in doctrinal matters, historical institutional continuity, or consensus among the faithful?',
    'Theological analysis of magisterial authority sources. Historical comparison: how has the magisterium claimed authority changed across Vatican I (papal infallibility in faith/morals), Vatican II (sensus fidelium, episcopal collegiality), and post-Vatican II reform documents? Sociological analysis: what proportion of world Catholics accept the magisterium''s teaching on indissolubility without qualification?',
    'If divinely mandated: the reading''s institutional extraction is theologically justified as the exercise of authorized authority. If based on historical continuity or consensus: the reading''s extraction becomes contingent on acceptance; loss of consensus undermines the constraint''s perceived legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_authority_grounding, conceptual, 'Grounding of hierarchical magisterial authority over doctrine').

omega_variable(
    pastoral_vs_doctrinal_authority_foreclosure,
    'Does the hierarchical indissolubility reading logically foreclose a reading that places pastoral authority (bishops and local communities) above doctrinal authority (magisterium''s interpretation of doctrine)? Or do these readings merely coexist in different institutional factions?',
    'Logical analysis: can a framework simultaneously hold (1) the magisterium has final authority over doctrine AND (2) pastoral communities have authority to adapt sacramental discipline to local contexts without magisterial approval? If the reading claims both, it is internally contradictory (foreclose). If it reserves doctrinal authority to the magisterium while allowing some pastoral discretion, they coexist.',
    'If foreclose: the civic/pastoral reading is logically impossible within a Church that accepts the hierarchical reading; one must be abandoned. If coexist: both readings persist as live alternatives held by different factions (reformers vs traditionalists) within the Church.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pastoral_vs_doctrinal_authority_foreclosure, conceptual, 'Whether hierarchical indissolubility forecloses pastoral-authority readings').

omega_variable(
    sacramental_efficacy_vs_institutional_gatekeeping,
    'Is the mechanism by which a divorced-remarried Catholic is excluded from Eucharist rooted in a genuine sacramental impediment (the sacrament of Eucharist cannot be efficacious for those in ''objective grave sin'') or in institutional discipline (the Church denies access as a punishment/enforcement mechanism)?',
    'Theological analysis: does the reading ground the exclusion in sacramental theology (the nature of the Eucharist itself) or in canon law (institutional rules for valid reception)? Historical analysis: how has the Church''s rationale for the exclusion shifted? (Pre-Vatican II: primarily sacramental; Vatican II: increasingly pastoral; Francis: increasing emphasis on access over penance.)',
    'If sacramental: the exclusion appears as a natural consequence of the constraint (sacrament cannot work if the recipient is in grave sin); suppression mechanism is not visible as ''institutional.'' If institutional: the suppression is clearly a choice — institutional actors decide who accesses the Eucharist; the constraint is transparently a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacramental_efficacy_vs_institutional_gatekeeping, conceptual, 'Whether Eucharistic exclusion is sacramentally grounded or institutionally enforced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__hierarchical_indissolubility_reading, 1965, 2005).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_hier_theater_t0, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(marr_hier_theater_t20, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(marr_hier_theater_t40, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(marr_hier_extract_t0, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(marr_hier_extract_t20, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(marr_hier_extract_t40, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marr_hier_suppress_t0, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(marr_hier_suppress_t20, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(marr_hier_suppress_t40, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__hierarchical_indissolubility_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament__civic_pastoral_reading).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, eucharistic_communion_access_gate).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, magisterial_authority_vatican_ii_reading).

% DUAL FORMULATION NOTE:
% The marriage_sacrament kernel decomposes into multiple constraint stories based on the reading adopted: hierarchical indissolubility (this story, ε≈0.58) vs. civic/pastoral reading (sibling story, ε≈0.25). The ε difference is not measurement ambiguity but a genuine structural difference — the readings instantiate different extraction mechanisms. The hierarchical reading enforces indissolubility through institutional gatekeeping; the civic reading treats marriage as a covenant subject to pastoral reinterpretation. Both stories share the same base text (Vatican II, Familiaris Consortio, subsequent papal documents) but read it differently, producing different constraint types and different victim/beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_sacrament__hierarchical_indissolubility_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
