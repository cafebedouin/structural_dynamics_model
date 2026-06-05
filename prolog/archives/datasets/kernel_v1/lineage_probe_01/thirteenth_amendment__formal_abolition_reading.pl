% ============================================================================
% CONSTRAINT STORY: thirteenth_amendment__formal_abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_thirteenth_amendment__formal_abolition_reading, []).

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
 *   constraint_id: thirteenth_amendment__formal_abolition_reading
 *   human_readable: Thirteenth Amendment: Formal Abolition Reading
 *   domain: constitutional_law/reconstruction
 *
 * SUMMARY:
 *   The Thirteenth Amendment formally abolished slavery as a legal
 *   institution, dissolving the property relation that constituted enslaved
 *   persons as chattels. The formal abolition reading interprets this as the
 *   complete scope of the amendment: it ended the legal status and prohibited
 *   involuntary servitude, but created no constitutional obligation to
 *   redress the social, economic, and political aftermath of slavery. This
 *   reading constrains the federal power to reach 'badges and incidents of
 *   slavery' — discriminatory practices, vagrancy laws, debt peonage, and
 *   violence — because those are framed as social phenomena, not slavery
 *   itself. The constraint exhibits the structure of a Tangled Rope: genuine
 *   coordination function (resolving the constitutional status of formerly
 *   enslaved persons through formal abolition) embedded in asymmetric
 *   extraction (limiting congressional reach to remedy the lived conditions
 *   of freedom). The reading has become increasingly performative as
 *   doctrinal authority — courts invoke the formal scope to limit civil
 *   rights legislation, even as the actual operative scope (through the 14th
 *   Amendment, statutory civil rights laws, and common law) has expanded far
 *   beyond it. This is the FORMAL ABOLITION READING of the contested kernel
 *   'thirteenth_amendment'. The sibling reading 'badges_of_servitude_reading'
 *   holds that Congress can constitutionally reach the badges and incidents
 *   of slavery, expanding the amendment's operative scope to encompass the
 *   social aftermath.
 *
 * KEY AGENTS:
 *   - Legally freed persons (1865+): Primary nominal beneficiary of the formal abolition; structurally trapped in social aftermath of slavery; experience the constraint as suppressive despite legal status
 *   - Former slaveholders / white property interests: Primary beneficiary of the formal reading; gain legal cover for conversion to wage labor and sharecropping; preserved ability to extract labor through legal instruments rather than property title
 *   - Freedmen's Bureau / Reconstruction federal agents: Secondary actors with constrained institutional power; tasked with enforcement but limited by federal withdrawal and state resistance; often complicit in sharecropping arrangements
 *   - Congress (39th, subsequent): Organized agent with formal power to legislate under the amendment; constrained in that power by courts interpreting the formal reading as limiting federal reach to the property relation only
 *   - Radical Republicans / Reconstruction coalition: Organized agents building alternative legal architecture (14th Amendment, Civil Rights Acts) to reach beyond the formal reading's scope
 *   - Federal courts (especially Supreme Court): Institutional interpreter of the amendment's scope; enforcing the formal reading through doctrine that limits congressional power; increasingly isolated as statutory law and lower court practice expand beyond the formal scope
 *   - Analytical observer: Risk of naturalizing the formal reading's scope boundaries as fixed constitutional meaning rather than recognizing them as a contingent interpretive choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(thirteenth_amendment__formal_abolition_reading, 0.38).
domain_priors:suppression_score(thirteenth_amendment__formal_abolition_reading, 0.48).
domain_priors:theater_ratio(thirteenth_amendment__formal_abolition_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(thirteenth_amendment__formal_abolition_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(thirteenth_amendment__formal_abolition_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(thirteenth_amendment__formal_abolition_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(thirteenth_amendment__formal_abolition_reading, tangled_rope).
narrative_ontology:human_readable(thirteenth_amendment__formal_abolition_reading, "Thirteenth Amendment: Formal Abolition Reading").
narrative_ontology:topic_domain(thirteenth_amendment__formal_abolition_reading, "constitutional_law/reconstruction").

domain_priors:requires_active_enforcement(thirteenth_amendment__formal_abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(thirteenth_amendment__formal_abolition_reading, 'd9dbb056-43f1-4ff5-bc0f-3ab1fff62b6e').
narrative_ontology:cs_kernel_codification('d9dbb056-43f1-4ff5-bc0f-3ab1fff62b6e', fixed_text).
narrative_ontology:cs_authority_grounding('d9dbb056-43f1-4ff5-bc0f-3ab1fff62b6e', lineage).
narrative_ontology:cs_interpretation_layer_present('d9dbb056-43f1-4ff5-bc0f-3ab1fff62b6e').
narrative_ontology:cs_reading_relation('d9dbb056-43f1-4ff5-bc0f-3ab1fff62b6e', thirteenth_amendment__badges_of_servitude_reading, coexists_with).
narrative_ontology:cs_axiom('d9dbb056-43f1-4ff5-bc0f-3ab1fff62b6e', foundational, slavery_property_relation_narrow).
narrative_ontology:cs_axiom_status(slavery_property_relation_narrow, holdable).
narrative_ontology:cs_axiom_grounding('d9dbb056-43f1-4ff5-bc0f-3ab1fff62b6e', slavery_property_relation_narrow, deontological).
narrative_ontology:cs_axiom('d9dbb056-43f1-4ff5-bc0f-3ab1fff62b6e', foundational, federal_enforcement_constrained_narrowly).
narrative_ontology:cs_axiom_status(federal_enforcement_constrained_narrowly, holdable).
narrative_ontology:cs_axiom_grounding('d9dbb056-43f1-4ff5-bc0f-3ab1fff62b6e', federal_enforcement_constrained_narrowly, conventional).
narrative_ontology:cs_reference_frame('d9dbb056-43f1-4ff5-bc0f-3ab1fff62b6e', legal_status_formal_freedom).
narrative_ontology:cs_drift_state('d9dbb056-43f1-4ff5-bc0f-3ab1fff62b6e', contemporary_post_1964, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d9dbb056-43f1-4ff5-bc0f-3ab1fff62b6e', '').
narrative_ontology:cs_kernel_id(thirteenth_amendment__formal_abolition_reading, thirteenth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(thirteenth_amendment__formal_abolition_reading, legally_freed_persons).
narrative_ontology:constraint_victim(thirteenth_amendment__formal_abolition_reading, claims_against_slavery_incidents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEWLY FREED PERSON (SNARE) — Legally freed from the property relation but trapped in the social, economic, and customary aftermath of slavery. The constraint formally protects only the abstract legal status; suppression of substantive freedom remains high through vagrancy laws, debt peonage, wage theft, and social violence. No exit option from the social structure that replaced slavery; experiences full extraction in lived conditions while bearing the burden of 'legally free' status without its material content.
constraint_indexing:constraint_classification(thirteenth_amendment__formal_abolition_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FORMER SLAVEHOLDER / WHITE PROPERTY INTEREST (ROPE) — Experiences the constraint as successful coordination: the property relation is legally dissolved, enabling rapid conversion to wage labor and sharecropping without prosecutorial risk. The formal reading protects the beneficiary's ability to extract labor through legal instruments (contracts, debt, law enforcement) rather than the now-prohibited property title. Arbitrage exit available — can relocate capital, shift to wage labor, adapt enforcement mechanisms to post-abolition law. Net beneficiary of the formal interpretation.
constraint_indexing:constraint_classification(thirteenth_amendment__formal_abolition_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: FREEDMEN'S BUREAU / FEDERAL RECONSTRUCTION AGENT (TANGLED ROPE) — Tasked with enforcing the legal abolition while constrained by federal withdrawal, state resistance, and undefined enforcement authority. Genuine coordination function: the Bureau coordinates transition from slavery to post-slavery labor systems, provides education and land access. But also constrained by lack of resources, political will, and clear mandate; often complicit in sharecropping arrangements. Moderate power; extracted from above (Congress limits scope), extracting from below (mediation of labor disputes favors landowners).
constraint_indexing:constraint_classification(thirteenth_amendment__formal_abolition_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FORMAL LEGAL DOCTRINE (PITON) — The constraint as doctrinal proposition persists through institutional inertia and textual authority despite degraded function. The reading has become largely performative in judicial doctrine: courts invoke the 'formal abolition' framing to limit congressional power to address slavery's incidents, but the framing itself is increasingly abandoned in practice (see Jones v. Alfred H. Mayer Co., 1968; modern civil rights legislation). The doctrine is maintained through citation and precedent even as its operative force has atrophied. Theater ratio reflects the gap between the formal statement ('slavery is abolished') and its functional scope (constrained to the narrow property relation, not its social aftermath).
constraint_indexing:constraint_classification(thirteenth_amendment__formal_abolition_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: BADGES OF SERVITUDE COALITION (SCAFFOLD) — Organized agents (abolitionists, Reconstruction Republicans, later civil rights advocates) see the formal reading as a temporary coordinate problem with a structural exit. The badges_of_servitude_reading — that Congress can reach private discrimination perpetuating slavery — is being built into alternative legal architecture (Civil Rights Act of 1875, 14th Amendment enforcement, eventual Civil Rights Act of 1964). This perspective sees the formal reading as superseded by emerging doctrine that expands Thirteenth Amendment scope. Low effective extraction because the coalition has agency and a path beyond the formal reading's constraints.
constraint_indexing:constraint_classification(thirteenth_amendment__formal_abolition_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL AUTHORITY STRUCTURE (TANGLED ROPE) — The constraint embodies a genuine coordination function (resolving the status of enslaved persons through constitutional amendment) alongside asymmetric extraction (limiting congressional authority to reach the social aftermath of slavery). The authority derives legitimacy from the formal text and the amendment process, but the constraint generates a persistent gap between the nominal scope ('slavery abolished') and the operative scope (only the property relation). Active enforcement required: courts must continually reject expansive interpretations of abolition to maintain the formal reading's boundaries. Constrained exit because alternative readings (badges of servitude) are already emerging and gaining doctrinal traction, but the formal reading persists through stare decisis and textual anchoring.
constraint_indexing:constraint_classification(thirteenth_amendment__formal_abolition_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal view, one might argue that the formal abolition reading captures something fixed and unchangeable: the constitutional text says 'slavery' and 'involuntary servitude,' and those refer to the legal property relation narrowly. The text's meaning is what it is; no court can expand 'slavery' to mean the social aftermath without textual stretching. This perspective risks naturalizing a contingent interpretive choice as a permanent feature of constitutional language. However, structural data reveals this as a false summit: the 'fixed meaning' framing is itself a reading, not a law of nature. The analytical observer must acknowledge that 'slavery' is a term whose scope has been genuinely contested — the formal reading is one interpretive commitment, not a crystalline semantic fact.
constraint_indexing:constraint_classification(thirteenth_amendment__formal_abolition_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thirteenth_amendment__formal_abolition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(thirteenth_amendment__formal_abolition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thirteenth_amendment__formal_abolition_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(thirteenth_amendment__formal_abolition_reading, TR),
    TR >= 0.70.

:- end_tests(thirteenth_amendment__formal_abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint embodies a real asymmetry: legally freed persons receive the formal status but not substantive remedies for slavery's aftermath, while former slaveholders receive legal cover for post-slavery extraction mechanisms (wage theft, debt peonage, sharecropping). But the extractiveness is not as high as a pure snare (≥0.46) because genuine coordination occurred: the amendment did resolve the fundamental constitutional question of slavery's status, and formal freedom, while incomplete, is structurally different from enslavement. The extractiveness increases over the interval (0.22 → 0.38 → 0.42) as the gap widens between the formal promise of abolition and the continued extraction through slavery's social aftermath. Suppression (0.48): Moderate-high. The formal reading creates barriers to federal remedy of the social aftermath: courts cite the reading to strike down civil rights legislation, to limit congressional power, and to restrict suits against private discrimination. But suppression is not total (≥0.60) because alternative legal channels (14th Amendment, statutory civil rights, state law) can still reach many slavery incidents; suppression operates through jurisdictional constriction and doctrinal limitation rather than absolute prohibition. Suppression increases over the interval (0.32 → 0.48 → 0.55) as the constraint's limiting function becomes more entrenched through precedent and doctrinal authority. Theater ratio (0.55): Moderate-high. The formal reading has become substantially performative in constitutional doctrine: it is invoked in authoritative spaces (Supreme Court opinions, law school teaching) to resolve the scope question, but the actual operative law has moved far beyond it. Congress legislates under the 14th Amendment, the Civil Rights Acts, and the Voting Rights Act as if the federal power to reach slavery's incidents exists; lower courts often honor this expanded scope; social movements treat the badges of servitude reading as the living constitutional standard. The formal reading persists as doctrinal citation (theater) while the badges reading dominates practice (function). Theater increases over the interval as the gap between doctrinal statement and operative authority widens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single constitutional text can be read to produce radically different classifications from different structural positions. The newly freed person sees a snare: legal status without substantive freedom, with no exit from either the formal constraint or the social aftermath that replaces it. The former slaveholder sees a rope: successful coordination enabling conversion to post-slavery labor systems. The Freedmen's Bureau sees tangled rope: genuine coordination function (managing the transition) embedded in constrained agency and often complicit outcomes. The formal legal doctrine, when examined closely, appears as a piton: the reading persists through doctrinal inertia and textual authority even as its operative force has atrophied and alternative readings (badges of servitude) have become dominant in practice. The organized Reconstruction coalition sees a scaffold: a temporary coordinate problem being solved by building alternative legal architecture (14th Amendment, civil rights legislation). The institutional authority structure (courts enforcing the formal reading) experiences it as a tangled rope: genuine role in constitutional interpretation mixed with constraints from doctrine and pressure from statutory law. The analytical observer risks seeing a mountain: treating the formal reading as a fixed, unchangeable meaning of the constitutional text. But the structural data reveals this as a false summit — the formal reading is a constructed interpretive choice that has been actively challenged and superseded in practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position relative to the extraction flow. Legally freed persons are victims with trapped exit: they cannot escape the social aftermath and cannot exit the constraint's jurisdictional reach through federal litigation (courts apply the formal reading to dismiss claims for remedy). Former slaveholders are beneficiaries with arbitrage exit: they can shift capital, convert to wage labor, adapt to post-slavery extraction instruments, and choose whether to litigate the amendment's scope. The Freedmen's Bureau is a moderate institutional actor with constrained exit: it has some agency within the constraint but is limited by federal withdrawal and state resistance. Congress has institutional power but constrained exit: it can legislate civil rights, but must navigate the formal reading's doctrinal barrier. Courts are the institutional interpreters with arbitrage exit through doctrinal choice: they can expand or narrow the reading's scope but currently choose (through judicial conservatism and originalism) to enforce the formal boundaries. The analytical observer risks identity lock: seeing the formal reading as a straightforward constitutional meaning rather than recognizing their own interpretive choice as contingent.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    slavery_term_scope_ambiguity,
    'Does the constitutional term ''slavery'' refer narrowly to the property relation alone, or does it encompass the social and economic system perpetuated after property abolition?',
    'Historical analysis of Reconstruction-era intent; textual comparison with other constitutional uses of ''slavery''; examination of whether Congress''s contemporaneous acts (Civil Rights Act of 1875, Freedmen''s Bureau legislation) treated abolition as extending beyond property',
    'If narrow property meaning: formal reading is the correct constitutional interpretation; badges of servitude reading requires 14th Amendment or independent statutory power. If expansive system meaning: formal reading misreads the text; badges of servitude reading is the correct interpretation of the 13th Amendment itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slavery_term_scope_ambiguity, conceptual, 'Whether ''slavery'' in the Thirteenth Amendment encompasses the social system or only the property relation').

omega_variable(
    reconstruction_era_congressional_intent,
    'What did the 39th Congress intend the Thirteenth Amendment to authorize Congress to reach through legislation?',
    'Congressional Globe records of debates on the 13th Amendment and subsequent civil rights bills; analysis of which restrictive practices Congress claimed power to address; timing and scope of Freedmen''s Bureau and Civil Rights Act of 1875',
    'If Congress intended broad reach: formal reading constrains the amendment contrary to its framers'' understanding; badges of servitude reading aligns with contemporaneous intent. If Congress intended narrow property abolition only: formal reading is historically grounded; badges of servitude reading extends beyond enacted intent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reconstruction_era_congressional_intent, empirical, 'Congressional intent regarding the scope of Thirteenth Amendment enforcement power').

omega_variable(
    formal_abolition_vs_constructed_reading,
    'Is the formal abolition reading a straightforward reading of the constitutional text, or a constructed interpretive choice that privileges formal status over substantive freedom?',
    'Comparison of interpretive methodologies: originalism, living constitutionalism, structural analysis; examination of whether the formal reading requires active interpretive moves (limiting ''slavery'' semantically, limiting congressional enforcement power) or passive acceptance of the text',
    'If straightforward: formal reading is the baseline; badges reading requires affirmative interpretive argument. If constructed: both readings are affirmative interpretive choices; the choice between them is not determined by the text alone but by theories of constitutional meaning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_abolition_vs_constructed_reading, conceptual, 'Whether the formal abolition reading is textually determined or interpretively constructed').

omega_variable(
    punishment_exception_enforcement_divergence,
    'Does the Thirteenth Amendment''s exception for ''punishment for crime'' create a separate extraction mechanism that this reading inadvertently preserves and legitimates?',
    'Historical analysis of convict leasing, mass incarceration, and racial disparities in criminal punishment post-1865; examination of whether the formal abolition reading has been used to justify the exception clause as constitutional rather than as a narrowly construed escape hatch',
    'If exception is actively enforced through criminal law: the formal reading creates structural cover for slavery-by-another-name through the exception clause. If exception is dormant or narrowly construed: the formal reading''s constraint is confined to the property relation as stated, without cascading into carceral extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(punishment_exception_enforcement_divergence, empirical, 'Whether the punishment exception creates a secondary extraction mechanism enabled by the formal abolition reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(thirteenth_amendment__formal_abolition_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1865, thirteenth_amendment__formal_abolition_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(theater_1875, thirteenth_amendment__formal_abolition_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement(theater_1885, thirteenth_amendment__formal_abolition_reading, theater_ratio, 20, 0.6).

% Extraction over time
narrative_ontology:measurement(base_extract_1865, thirteenth_amendment__formal_abolition_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(base_extract_1875, thirteenth_amendment__formal_abolition_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(base_extract_1885, thirteenth_amendment__formal_abolition_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(suppress_1865, thirteenth_amendment__formal_abolition_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(suppress_1875, thirteenth_amendment__formal_abolition_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(suppress_1885, thirteenth_amendment__formal_abolition_reading, suppression_requirement, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(thirteenth_amendment__formal_abolition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(thirteenth_amendment__formal_abolition_reading, thirteenth_amendment__badges_of_servitude_reading).

% DUAL FORMULATION NOTE:
% The Thirteenth Amendment constraint family decomposes into two readings with different operative scopes and different extractiveness values. The formal_abolition_reading (this constraint, ε=0.38) models the constraint as limiting federal power to the property relation. The badges_of_servitude_reading (ε=higher, coordinate with enforcement_expansion dynamics) models the constraint as authorizing federal power to reach slavery's social aftermath. These are not the same constraint viewed from different angles — they have different ε values, different beneficiary/victim structures, and different operative doctrinal consequences. The formal reading constrains federal remedy; the badges reading expands it. They are linked as sibling readings of the same kernel (the amendment's text and scope) but constitute separate constraints with separate structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(thirteenth_amendment__formal_abolition_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
