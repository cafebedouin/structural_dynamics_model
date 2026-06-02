% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__crown_cession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__crown_cession_reading, []).

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
 *   constraint_id: treaty_authority_cession__crown_cession_reading
 *   human_readable: Treaty Authority Cession (Crown Sovereignty Reading)
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   Te Tiriti o Waitangi (Treaty of Waitangi, 1840) is a contested kernel
 *   grounding constitutional authority in New Zealand. The
 *   crown_cession_reading interprets the treaty as a transfer of full
 *   sovereignty ('kāwanatanga') from Māori iwi to the British Crown,
 *   extinguishing or subordinating indigenous customary governance authority
 *   (rangatiratanga) and legitimizing Crown legislative supremacy and settler
 *   land alienation. This reading has been the dominant institutional
 *   interpretation since 1840, embedded in case law, legislation, and
 *   colonial administrative practice. However, its authority base has eroded
 *   significantly since the 1960s through Māori political mobilization,
 *   historical scholarship demonstrating semantic and translational
 *   ambiguities in the treaty text, and Crown adoption of alternative
 *   readings grounded in bicultural authority division and Treaty principles.
 *   The constraint exhibits all six classification types from different
 *   perspectives: Māori iwi experience it as a snare (trapped by a cession
 *   reading they dispute); the Crown experiences it as a rope (legitimate
 *   governance coordination); the settler judicial system experiences it as a
 *   piton (ritualized invocation with declining functional necessity);
 *   settler landholders experience it as a tangled rope (benefiting from
 *   Crown-derived title while facing increasing rangatiratanga claims);
 *   constitutional reform coalitions experience it as a scaffold (organized
 *   pressure eroding the reading toward bicultural settlement); and the
 *   analytical observer risks naturalizing the reading as an unchangeable
 *   legal fact (false summit). The measurements track the constraint's
 *   lifecycle: high initial extractiveness and suppression (immediate
 *   post-treaty enforcement through land wars and legal subordination,
 *   1840–1870); gradual decline in enforcement requirements and rise in
 *   theatrical justification (1870–2020) as Crown legitimacy transitions from
 *   pure coercion to institutionalized doctrinal authority; current state
 *   showing continued extraction but increasing visibility of the reading's
 *   contingency (2020–present).
 *
 * KEY AGENTS:
 *   - Crown Executive and Legislature: Primary institutional beneficiary (institutional/arbitrage) — derives legislative supremacy, territorial control, and taxation authority from the cession reading. Can arbitrage by reinterpreting the reading or negotiating selective co-governance arrangements.
 *   - Māori Iwi: Primary victims (powerless/trapped) — subordinated under the reading's cession logic; land alienated; customary governance authority extinguished or marginalized. No exit option under this reading's terms.
 *   - Settler Landholders: Secondary beneficiaries and mixed actors (moderate/constrained) — benefit from Crown-derived title legitimacy but face increasing rangatiratanga claims, settlement obligations, and redress costs.
 *   - Imperial/Settler Judiciary and Legal Academy: Institutional ritualists (institutional/arbitrage) — maintain the reading through repeated doctrinal invocation despite eroding historical basis. Piton perspective: declining functional necessity, increasing performative content.
 *   - Constitutional Reform Coalition: Organized challengers (organized/constrained) — progressive legal scholars, iwi leadership, human rights advocates pushing toward bicultural readings and rangatiratanga recognition. See the cession reading as transitional, maintain pressure for reading displacement.
 *   - Analytical Observer: Civilizational vantage (analytical/analytical) — risks naturalization of the cession reading as neutral legal fact rather than contingent historical interpretation grounded in beneficiary interests.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, 0.68).
domain_priors:suppression_score(treaty_authority_cession__crown_cession_reading, 0.72).
domain_priors:theater_ratio(treaty_authority_cession__crown_cession_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__crown_cession_reading, snare).
narrative_ontology:human_readable(treaty_authority_cession__crown_cession_reading, "Treaty Authority Cession (Crown Sovereignty Reading)").
narrative_ontology:topic_domain(treaty_authority_cession__crown_cession_reading, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__crown_cession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__crown_cession_reading, '9c695ba3-e709-4a94-98ee-d1a97b9235ee').
narrative_ontology:cs_kernel_codification('9c695ba3-e709-4a94-98ee-d1a97b9235ee', fixed_text).
narrative_ontology:cs_authority_grounding('9c695ba3-e709-4a94-98ee-d1a97b9235ee', extraction).
narrative_ontology:cs_interpretation_layer_present('9c695ba3-e709-4a94-98ee-d1a97b9235ee').
narrative_ontology:cs_reading_relation('9c695ba3-e709-4a94-98ee-d1a97b9235ee', treaty_authority_cession__rangatiratanga_retention_reading, forecloses).
narrative_ontology:cs_reading_relation('9c695ba3-e709-4a94-98ee-d1a97b9235ee', treaty_authority_cession__biculturalism_reading, influences).
narrative_ontology:cs_axiom('9c695ba3-e709-4a94-98ee-d1a97b9235ee', foundational, kawanatanga_absolute_sovereignty).
narrative_ontology:cs_axiom_status(kawanatanga_absolute_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('9c695ba3-e709-4a94-98ee-d1a97b9235ee', kawanatanga_absolute_sovereignty, conventional).
narrative_ontology:cs_axiom('9c695ba3-e709-4a94-98ee-d1a97b9235ee', foundational, indigenous_authority_extinguished).
narrative_ontology:cs_axiom_status(indigenous_authority_extinguished, overridden).
narrative_ontology:cs_axiom_grounding('9c695ba3-e709-4a94-98ee-d1a97b9235ee', indigenous_authority_extinguished, deontological).
narrative_ontology:cs_reference_frame('9c695ba3-e709-4a94-98ee-d1a97b9235ee', crown_legislative_supremacy).
narrative_ontology:cs_drift_state('9c695ba3-e709-4a94-98ee-d1a97b9235ee', contemporary_bicultural_institutional_moment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9c695ba3-e709-4a94-98ee-d1a97b9235ee', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__crown_cession_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, crown_executive).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, settler_land_claimants).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_iwi).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, indigenous_customary_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MĀORI IWI (SNARE) — Trapped by the reading's core claim that 'kāwanatanga' (governance authority) was transferred wholly to the Crown at treaty signature. Under this reading, iwi retain no legitimate claim to legislative authority or land tenure outside Crown-granted allowances. Exit options are nil — customary authority structures are foreclosed by the treaty's supposed cession clause. The constraint extracts (through land alienation and subordination of governance) with minimal coordination benefit to the trapped party.
constraint_indexing:constraint_classification(treaty_authority_cession__crown_cession_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CROWN EXECUTIVE (ROPE) — Experiences the constraint as legitimate governance coordination. The reading casts the treaty as a legal transfer of authority — the Crown sees this as a Rope: solving the coordination problem of unified law and territorial control. The Crown can arbitrage by interpreting the treaty's terms, amending legislation, or renegotiating with compliant iwi leadership. Net beneficiary through consolidated legislative and executive power.
constraint_indexing:constraint_classification(treaty_authority_cession__crown_cession_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: IMPERIAL/SETTLER LEGISLATIVE APPARATUS (PITON) — The Crown's legal system maintains the crown_cession_reading as canonical through repeated invocation in case law and legislation, but the reading's empirical basis has eroded. The theater ratio (0.58) reflects that the Crown's performative legitimation of the reading through statute and judicial doctrine persists despite historical scholarship demonstrating ambiguities in the treaty text itself. The apparatus continues ritualized invocation (statutes, court opinions affirming Crown authority) with declining functional necessity — the reading has become institutionalized through inertia.
constraint_indexing:constraint_classification(treaty_authority_cession__crown_cession_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SETTLER LANDHOLDERS (TANGLED ROPE) — Benefit from the Crown's legitimate title derived from the crown_cession_reading — their land claims rest on Crown-granted tenure, which depends on the treaty reading's validity. But they also face increasing constraints as iwi assert customary land claims, environmental authority, and historical grievance settlements. The constraint coordinates settler title legitimacy while extracting (through litigation costs, settlement obligations, and redress claims). Moderate extraction because they have agency (property rights, political voice) and real benefits (secure tenure).
constraint_indexing:constraint_classification(treaty_authority_cession__crown_cession_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: CONSTITUTIONAL REFORM COALITION (SCAFFOLD) — Organized agents (progressive legal scholars, iwi coalition, human rights advocates) view the crown_cession_reading as a temporary constraint being superseded by competing readings grounded in bicultural settlement principles and rangatiratanga retention. This perspective sees the crown_cession_reading as historically dominant but now eroding through legislative amendment (Treaty Settlements Act 1974 onwards, historical claims commissions), Crown apology and reparations, and increasing Māori political influence. The sunset is not a formal clause but a structural erosion through political pressure and reframing. Effective extraction for this perspective is low because they see a pathway (constitutional settlement, co-governance models) that marginalizes the cession reading.
constraint_indexing:constraint_classification(treaty_authority_cession__crown_cession_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, this reading risks presenting itself as a neutral legal fact — 'the treaty simply transferred authority; this is what law is; sovereignty cannot be divided.' This perspective naturalizes what is actually a contested historical and textual interpretation. The natural law framing serves the beneficiaries by making the cession reading appear inevitable rather than contingent. The engine's false summit detector will flag this: the beneficiary/victim structure and the Crown's active enforcement of the reading contradict the mountain classification.
constraint_indexing:constraint_classification(treaty_authority_cession__crown_cession_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__crown_cession_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(treaty_authority_cession__crown_cession_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(treaty_authority_cession__crown_cession_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(treaty_authority_cession__crown_cession_reading, TR),
    TR >= 0.70.

:- end_tests(treaty_authority_cession__crown_cession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, declining. The cession reading extracts substantial value from Māori iwi through land alienation, governance subordination, and cultural authority suppression. The reading legitimizes Crown seizure of ~95% of pre-treaty Māori-held land and enables legislative monopoly over iwi. However, extractiveness has declined from 0.85 (immediate post-treaty period, 1840–1870) to current 0.68 as land claims settlements, co-governance pilots, and rangatiratanga recognition statutes partially reverse extraction mechanisms. The baseline (0.68) reflects contemporary Crown-Māori dynamics where the reading remains dominant but increasingly contested and partially superseded. Suppression (0.72): High, declining. Enforced through coercive state apparatus (courts, police, land registration law, electoral suppression) for 130+ years (1840–1970); suppression requirement remains high because organized Māori resistance (political parties, iwi coalitions, protest movements) has grown since 1960s, necessitating continued legal enforcement and doctrinal invocation. Measurement trajectory (0.89 → 0.78 → 0.72) reflects increasing Māori political capacity requiring more intensive suppression. Theater ratio (0.58): Moderate-high, rising. Initial period (1840–1870) relied on raw coercion (land wars, police suppression) — low theater ratio. Post-1870 period increasingly relied on judicial doctrine, statutory authority, and legal ritual to justify the cession reading — rising theater ratio. Contemporary period (2020–present) shows highest theater: Crown and settler judiciary repeatedly invoke the reading while simultaneously adopting co-governance and rangatiratanga recognition — performative maintenance of cession logic alongside practical displacement. Claimed type (Snare): Captures the empirical reality from the trap victim's perspective — Māori iwi experience it as pure extraction with minimal coordination benefit. From Crown and settler perspectives, it appears as rope or tangled rope, reflecting their beneficiary position. The analytical observer (mountain perspective) risks naturalizing the snare as inevitable.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The Māori iwi (snare perspective, trapped/powerless) experience a loss of authority and territory with no exit — the constraint's terms foreclose customary governance and legitimate land alienation. The Crown (rope perspective, institutional/arbitrage) experiences legitimate governance coordination and consolidated authority — the constraint solves the problem of unified law. The settler judiciary (piton perspective, institutional/arbitrage) maintains ritualized doctrinal invocation despite declining functional necessity — the reading persists through institutional inertia, not active justification. Settler landholders (tangled rope perspective, moderate/constrained) benefit from Crown-derived title but face increasing litigation and settlement costs — mixed extraction. Constitutional reformers (scaffold perspective, organized/constrained) experience the cession reading as transitional, being superseded through legislative amendment and political pressure — low effective extraction because they see the sunset. The analytical observer (mountain perspective, analytical/analytical) risks naturalizing the reading as an unchangeable legal fact, but the structural data (clear beneficiaries, organized victims, declining suppression requirement) contradicts the mountain classification. This gap reveals that the 'inevitability' of the cession reading is a framing imposed by beneficiaries, not a property of the constraint itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural relationships: beneficiary status (Crown, settler landholders) → low d → negative or low f(d) → low/negative χ (they experience the constraint as rope or beneficial coordination). Victim status (Māori iwi, indigenous authority) → high d → high f(d) → high χ (they experience maximum extraction). The Crown's arbitrage exit option (can reinterpret the treaty, negotiate settlements, adopt co-governance) produces lower d than Māori's trapped exit (no exit under the cession reading's own terms). The settler judiciary's arbitrage option (can cite the reading in judgments, maintain doctrinal authority) keeps them beneficiary-positioned despite declining legitimacy. Organized agents (reform coalition, iwi coalitions) with constrained exit experience moderate d because they have collective agency and resource capacity, even though they are epistemically trapped by the reading's dominance. The measurement trajectory shows directionality stability over time: beneficiaries remain low d (they maintain arbitrage capacity), victims remain high d (their exit constraints persist), organized challengers remain moderate d (capacity to pressure but not to unilaterally override).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kawanatanga_semantic_ambiguity,
    'Does ''kāwanatanga'' in the Māori text of Te Tiriti refer to absolute sovereignty (full governing authority) or to executive governance authority compatible with retained iwi rangatiratanga?',
    'Etymological and historical linguistic analysis of kāwanatanga usage in Māori sources contemporaneous with the treaty; cross-examination of how Māori signatories and their descendants understood the term; comparison with other Polynesian cognates and their juridical contexts.',
    'If kāwanatanga = absolute sovereignty: crown_cession_reading is semantically defensible. If kāwanatanga = executive governance only: rangatiratanga_retention_reading gains textual footing; bifurcated authority becomes plausible; extraction mechanism collapses toward rope or tangled_rope from Māori perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kawanatanga_semantic_ambiguity, empirical, 'Semantic scope of kāwanatanga: absolute or limited sovereignty').

omega_variable(
    treaty_signing_authority_legitimacy,
    'Did Māori signatories possess legitimate authority to cede full sovereignty, and did they understand themselves as doing so?',
    'Archival analysis of signings: who signed (chiefs, tohunga, representatives), what authority they claimed, what communication occurred. Māori oral histories and whānau records of treaty negotiations. Post-signing Māori assertions of continued rangatiratanga and how they were negotiated or suppressed.',
    'If signatories lacked authority or understood cession differently: the treaty was void ab initio under international law principles (consent-based legitimacy). Crown_cession_reading becomes unjustified even if textually plausible. Extraction mechanism is fully visible: enforced through unequal power, not legal validity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_signing_authority_legitimacy, empirical, 'Whether treaty signatories legitimately ceded full sovereignty').

omega_variable(
    english_crown_soveriegn_definition,
    'What did ''sovereignty'' mean to Crown negotiators and English legal doctrine at the time of the treaty (1840)?',
    'Historical analysis of Crown instructions to negotiators, contemporary British colonial law doctrines, comparable treaties (e.g., American Indian treaties, Australian colonial arrangements). What did the Crown claim to be acquiring at the time?',
    'If Crown doctrine allowed for divided sovereignty or residual indigenous authority: the cession reading was not the only plausible outcome even under Crown law at the time. If Crown doctrine required absolute sovereignty: the reading is historically consistent but its universality depends on international legal principles that Māori did not ratify.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(english_crown_soveriegn_definition, empirical, 'Contemporary Crown legal conception of sovereignty transfer').

omega_variable(
    enforcement_mechanism_equity,
    'To what extent is the crown_cession_reading maintained through coercive enforcement (courts, police, land registration law) versus genuine Māori consent to the reading''s legitimacy?',
    'Historical analysis of Crown enforcement actions against Māori assertions of rangatiratanga (land wars, police suppression, court injunctions). Longitudinal tracking of Māori resistance to the cession reading and Crown response. Post-WWII Māori political mobilization and whether Crown has voluntarily reframed the reading or only done so under organized pressure.',
    'If high coercion + low consent: the constraint is pure snare (reading maintained through suppression). If significant consent: the constraint has coordination function (tangled_rope or even rope from some Māori perspectives). If consent is increasing: scaffold perspective (sunset) is empirically supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_equity, empirical, 'Balance of coercive enforcement vs. voluntary consent in maintaining the cession reading').

omega_variable(
    biculturalism_as_reading_displacement,
    'Is the biculturalism_reading a genuine alternative reading of the same treaty kernel, or does it displace and supersede the crown_cession_reading as authoritative?',
    'Textual and institutional analysis: do contemporary Crown policies and legislation operationalize bicultural authority division (coexistence of Crown and Māori governance), or do they operationalize cession with Crown-granted Māori consultation rights (cession plus advisory input)? Examine Treaty Settlements Act amendments, co-governance legislation (e.g., Hauraki Collective Settlement Act, Three Waters Entities Bill), and judicial doctrine (e.g., principles of the Treaty in case law).',
    'If biculturalism coexists: both readings remain live; the constraint exhibits coexistence dynamics (different parties hold different readings simultaneously). If biculturalism displaces: the cession reading is being formally abandoned by the Crown itself; the constraint''s authority base is eroding; mandatrophy is resolved via institutional transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biculturalism_as_reading_displacement, conceptual, 'Whether biculturalism displaces or coexists with the cession reading').

omega_variable(
    maori_political_coalition_capacity,
    'Can Māori political organizations sustain unified pressure for rangatiratanga_retention_reading and constitutional reframing, or does iwi-level fragmentation prevent coalition maintenance?',
    'Political science analysis: tracking of Māori political coalitions (iwi coalitions, pan-tribal organizations, political parties), their longevity, resource bases, and legislative impact. Comparison with periods of high fragmentation vs. unified pressure. Electoral analysis of Māori voter mobilization and political party platforms.',
    'If coalition capacity is high and sustained: scaffold perspective (organized pressure eroding the cession reading) is structurally sound. If fragmentation is endemic: the reading may persist despite low legitimacy because organized resistance cannot maintain. Affects timeline and confidence in sunset clause.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maori_political_coalition_capacity, empirical, 'Māori political coalition capacity to sustain pressure for reading reframing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__crown_cession_reading, 0, 180).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tac_crown_theater_t0, treaty_authority_cession__crown_cession_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tac_crown_theater_t40, treaty_authority_cession__crown_cession_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(tac_crown_theater_t180, treaty_authority_cession__crown_cession_reading, theater_ratio, 180, 0.58).

% Extraction over time
narrative_ontology:measurement(tac_crown_extract_t0, treaty_authority_cession__crown_cession_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(tac_crown_extract_t40, treaty_authority_cession__crown_cession_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement(tac_crown_extract_t180, treaty_authority_cession__crown_cession_reading, base_extractiveness, 180, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tac_crown_suppress_t0, treaty_authority_cession__crown_cession_reading, suppression_requirement, 0, 0.89).
narrative_ontology:measurement(tac_crown_suppress_t40, treaty_authority_cession__crown_cession_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(tac_crown_suppress_t180, treaty_authority_cession__crown_cession_reading, suppression_requirement, 180, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__crown_cession_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__biculturalism_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, waitangi_tribunal_authority).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, land_claims_settlement_process).

% DUAL FORMULATION NOTE:
% The treaty_authority_cession kernel has three competing structured readings with substantially different extractiveness profiles. The crown_cession_reading (this constraint) has ε=0.68 and functions as a snare from Māori perspective. The rangatiratanga_retention_reading has ε≈0.25 (minimal extraction, maintains divided authority) and functions as rope or mountain from Māori perspective. The biculturalism_reading has ε≈0.35 (moderate extraction with coordination function) and functions as tangled rope. These are not the same constraint viewed from different angles — they are different constraints with different beneficiary/victim structures and different base extraction values. Each reflects a coherent interpretation of the treaty kernel's authority structure. Network links enable analysis of how the three readings compete for institutional dominance and how belief in one reading affects the viability of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(treaty_authority_cession__crown_cession_reading, institutional, 0.08).
constraint_indexing:directionality_override(treaty_authority_cession__crown_cession_reading, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
